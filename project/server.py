import sys
import asyncio
import aiohttp
import json
import time

API_KEY = 'XXX'

clients = {}

connection = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday'],
    'Welsh': ['Holiday']
}

port_number = {
    'Goloman': 11630,
    'Hands': 11631,
    'Holiday': 11632,
    'Welsh': 11633,
    'Wilkes': 11634
}


async def handle_clients(reader, writer):
    r_data = await reader.readline()
    receive_time = time.time()
    r_data = r_data.decode()
    log_file.write("Received the input: " + r_data)

    if check_changeloc(r_data) == 1:
        our_msg = r_data.strip().split()
        if our_msg[1] in clients:
            if our_msg[3] > clients[our_msg[1]][3]:
                clients[our_msg[1]] = our_msg
                a = our_msg[1:]
                asyncio.ensure_future(flood_fill('CHANGELOC {0}\n'.format(' '.join(a)), server_name))
        else:
            clients[our_msg[1]] = our_msg
            b = our_msg[1:]
            asyncio.ensure_future(flood_fill('CHANGELOC {0}\n'.format(' '.join(b)), server_name))

    else:
        output = await generate_message(r_data, receive_time)
        log_file.write("Sending the output: " + output)
        writer.write(output.encode())
        await writer.drain()


def check_changeloc(i_data):
    i_data = i_data.strip().split()
    if i_data[0] == "CHANGELOC":
        if len(i_data) != 6:
            return -1
        else:
            return 1
    else:
        return -1


async def generate_message(msg, received_time):
    input_msg = msg.strip().split()
    c_message = check_message(input_msg)
    #print (c_message)
    if c_message == "IAMAT":
        new_info = None
        if get_location(input_msg[2]) is not None:
            new_info = [input_msg[0], input_msg[1], input_msg[2], input_msg[3], str(received_time), server_name]

        if new_info is None:
            output_msg = "? {0}".format(msg)
            return output_msg
        else:
            diff_time = received_time - float(input_msg[3])
            if diff_time < 0:
                diff_time = "-" + str(diff_time)
            else:
                diff_time = "+" + str(diff_time)
            clients[input_msg[1]] = new_info
            m = input_msg[1:]
            output_msg = ("AT {0} {1} {2}\n".format(server_name, diff_time, ' '.join(m)))
            asyncio.ensure_future(flood_fill('CHANGELOC {0}\n'.format(' '.join(m)), server_name))
            return output_msg

    elif c_message == "WHATSAT":
        if input_msg[1] in clients:
            radius = input_msg[2]
            radius = float(radius) * 1000
            client = clients[input_msg[1]]
            location = get_location(client[2])
            location = str(location[0]) + "," + str(location[1])

            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(
                API_KEY, location, radius)

            diff_time = float(client[4]) - float(client[3])
            if diff_time < 0:
                diff_time = "-" + str(diff_time)
            else:
                diff_time = "+" + str(diff_time)
            output_msg = "AT {0} {1} {2} {3} {4}\n".format(client[5], diff_time, client[1], client[2], client[3])

            async with aiohttp.ClientSession() as session:
                async with session.get(url) as our_response:
                    n_results = int(input_msg[3])
                    json_response = await our_response.json()
                    json_response['results'] = json_response['results'][:n_results]
                    output_msg += json.dumps(json_response, indent=3)
                    output_msg += "\n\n"
            return output_msg
        else:
            output_msg = "? {0}".format(msg)
            return output_msg
    else:
        output_msg = "? {0}".format(msg)
        return output_msg


def check_message(msg):
    r_str = "invalid"
    process_time = None
    radius = None
    n_items = None
    if len(msg) < 1:
        return r_str
    elif msg[0] == "IAMAT":
        if len(msg) != 4:
            return r_str
        else:
            if get_location(msg[2]) is None:
                return r_str
            else:
                try:
                    process_time = float(msg[3])
                except:
                    pass
                if process_time is not None:
                    r_str = "IAMAT"
                    return r_str
                else:
                    return r_str

    elif msg[0] == "WHATSAT":
        if len(msg) != 4:
            return r_str
        else:
            try:
                radius = float(msg[2])
            except:
                pass
            if radius is not None:
                if radius <= 0 or radius > 50:
                    return r_str
                else:
                    try:
                        n_items = int(msg[3])
                    except:
                        pass
                    if n_items is not None:
                        if n_items <= 0 or n_items > 20:
                            return r_str
                        else:
                            r_str = "WHATSAT"
                            return r_str
                    else:
                        return r_str

            else:
                return r_str
    else:
        return r_str


def get_location(input_location):
    r_location = None
    a = 0
    n = len(input_location)
    for i in range(n):
        if input_location[i] == '-' or input_location[i] == '+':
            a += 1
    if a != 2:
        return r_location
    elif input_location[-1] == '-' or input_location[-1] == '+':
        return r_location
    elif input_location[0] != '-' and input_location[0] != '+':
        return r_location
    else:
        try:
            h_index = 0
            for i in range(n):
                if input_location[i] == '-' or input_location[i] == '+':
                    h_index = i

            latitude = input_location[:h_index]
            longitude = input_location[h_index:]
            #print (latitude)
            #print (longitude)
            r_location = float(latitude), float(longitude)
        except:
            pass
        return r_location


async def flood_fill(m, s):
    for t in connection[s]:
        log_file.write("Opening connection with server named {0} at port number {1}...".format(t, port_number[t]))
        try:
            reader, writer = await asyncio.open_connection('127.0.0.1', port_number[t], loop=loop)
            log_file.write("Successfully built the connection\n")
            writer.write(m.encode())
            await writer.drain()
            writer.close()
        except:
            log_file.write("Failed to connect\n")
            pass


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Error: need the name of server!")
        exit()

    global server_name
    server_name = sys.argv[1]
    if server_name not in port_number:
        print("Error: invalid server name")
        exit()

    global log_file
    log_name = server_name + "_log.txt"
    log_file = open(log_name, "w+")

    global loop
    loop = asyncio.get_event_loop()
    conn_clients = asyncio.start_server(handle_clients, '127.0.0.1', port_number[server_name], loop=loop)
    server = loop.run_until_complete(conn_clients)

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        print("\n" + server_name + " closed at ", time.time())
        pass

    # close the server
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
    # close our log file
    log_file.close()


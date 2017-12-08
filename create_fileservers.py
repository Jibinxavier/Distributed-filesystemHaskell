import yaml
import copy
import sys
fileserver_config = "./base_dockercompose/base_fileserver.yml"
def get_fserver_config(fileserver_config):

    with open(fileserver_config, 'r') as stream:
        try:
            return yaml.load(stream)
        except yaml.YAMLError as exc:
            print(exc)
def write_fserver_config(config,custom_config):
    with open(custom_config, 'w') as file:
        try:
            yaml.dump(config, file, default_flow_style=False)
        except yaml.YAMLError as exc:
            print(exc)
def main(n):

    config = get_fserver_config(fileserver_config)

    final_config = {}
    fserv_start_port = 8000
    db_start_port = 27090
    for i in range(n):
        fserv_port = fserv_start_port + i
        database_name = "database_" + str(i)
        fileserver = copy.deepcopy(config["fileserver"])
        fileserver["environment"] = [
            "MONGODB_IP=database_{}".format(i),
            "FILESERVER_Port={}".format(fserv_port),
            "FILESERVER_Name=f_1",
        ]
        fileserver["ports"] = ["{}:{}".format(fserv_port,fserv_port)]
        fileserver["container_name"] = "fileserver_{}".format(i)
        fileserver["links"] = [database_name]
        database = copy.deepcopy(config["database"])
        database["ports"]  = ["{}:27017".format(db_start_port +i  )]

        final_config["fileserver_" + str(i)] = fileserver
        final_config[database_name] =database


    write_fserver_config(final_config, "./fileserver/docker-compose.yml")
    


if __name__ =="__main__":
    main(int(sys.argv[1]))
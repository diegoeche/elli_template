all_servers = []

role :app, *all_servers
role :web, *all_servers

set :branch, "master"

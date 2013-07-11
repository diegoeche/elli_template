elli_template
=============

A simple template project to help you getting started creating Erlang Web
servers with tests and environment-based configurations.

## Getting Started ##

    git clone git://github.com/diegoeche/elli_template.git
    cd elli_template
    rebar get-deps && rebar compile

Congratulations! You have just created, compiled and tested your application.

## Running, Stopping and Attaching ##

Compared to other OTP applications, this template is not based on a `rel` folder. In
order to run the application simply run:

    ./script/control (start|stop|attach|console)

## Testing ##

To test, simply:

    ./script/test

## Deploying through Capistrano ##

Please refer to the
[Capistrano documentation](https://github.com/capistrano/capistrano) for questions
regarding capistrano. In any other case, the procedure to deploy should be:

1. Create a folder in the remote server(s) under:

    /var/www/[[application]]

You can change this location by editing `deploy.rb`

2. Edit the files under:

    ./config/deploy/*.rb

And set the servers to the remote servers you want to deploy to.

3. `cap [[environment]] deploy:setup`

4. `cap [[environment]] deploy`

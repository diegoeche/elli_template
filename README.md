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

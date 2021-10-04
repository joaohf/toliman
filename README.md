toliman
=====

An OTP application for Erlang Distributed Application demonstration purpose.

References
-----

For further details check out the following references:

* [9 Distributed Applications](https://erlang.org/doc/design_principles/distributed_applications.html)
* [Distribunomicon](https://learnyousomeerlang.com/distribunomicon)
* [erlang-questions Re: Distributed application takeover](http://erlang.org/pipermail/erlang-questions/2011-March/057323.html)
* [kernel application common test](https://github.com/erlang/otp/blob/master/lib/kernel/test/application_SUITE.erl)

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 ct --name ct

# statsd-erlang

an Erlang client for [statsd](https://github.com/etsy/statsd) that is based on OTP's gen_server behaviour.

## Usage

Add statsd-erlang to your application file like this:

```erlang
{mod, {statsd, []}}
```

You can specify the host, port and pool size in the configuration, too:

```erlang
{env, [{pool_size, 5},
       {host, {"localhost", 8125}}
      ]}
```

The above values are set by default, so you only have to specify them if you want to use others.

```erlang
statsd:start().

statsd:increment("foo.bar").
statsd:count("foo.bar", 5).
statsd:timing("foo.bar", 2500).
```

## Development

To compile the source, use [rebar](https://github.com/basho/rebar/).

## Author

Original author: Dominik Liebler <liebler.dominik@gmail.com>
Worker pool rework: Martin Rehfeld <martin.rehfeld@wooga.com>

## License

(The MIT License)

Copyright (c) 2013 Dominik Liebler

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# exs1024: xorshift1024star PRNG for Erlang

xorshift64star is a 64-bit PRNG of (2^1024-1) period.

See <http://xorshift.di.unimi.it/> for the further details.

A preliminary test shows the exs1024 functions takes roughly x2 of execution time of random module on a x86_64 or amd64 architecture environment.

## Author

Algorithm by Sebastiano Vigna, made public domain.

Programmed by Kenji Rikitake.

## LICENSE

MIT License.

(The code under `c-example` directory is licensed CC0/public domain.)

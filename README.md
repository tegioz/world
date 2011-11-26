Simulated world of carrots, rabbits and wolves
===

This is my first attempt at writting something in Erlang.

"We had to implement a simulated world inhabited by carrots, rabbits, and wolves. Rabbits would roam this world eating carrots that grew in random patches. When they had eaten enough carrots, the rabbits would get fat and split in two. Wolves ran around eating up the rabbits; if they managed to catch and eat enough rabbits, they would also get fat and split. Rabbits and wolves within a certain distance of each other would broadcast information on food and predators. If a rabbit found a carrot patch, other rabbits would quickly join him. If a wolf found a rabbit, the pack would start chasing it."

### Customize your world:

Edit world\_types.hrl and modify the world\_info record (max\_x and max\_y are the dimensions of the world):

-record(world\_info, {carrots=20,rabbits=2,wolves=2,max\_x=10,max\_y=10}).

### Build:

Compile (requires Erlang):

    erlc *.erl

### Run:

Launch Erlang runtime:

    erl

Start your own world:
 
    application:start(world).

### And now follow your rabbits!!!

TODO
---

  - Graphical interface

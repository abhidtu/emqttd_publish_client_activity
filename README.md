This is a work in progress relating to publish/(send to a messaging system) selectable emqttd message states

Use cases: 
publish messages on to

1> "SYSTEM/presence/connected" whene a client connects broker,

2> "SYSTEM/presence/disconnected" whene a client connects broker,

3> "SYSTEM/subscription" when a client subscribes/unsubscribes to a channel,

4> "SYSTEM/message" when a client publishes a message to a channel,

5> "SYSTEM/msgack" when a client acknowledges a messages

with a configurable QoS value

The data published on above events can be used to track message and client's activity in realtime.

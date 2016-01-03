##emqttd_publish_message_states

This is a work in progress relating to publish/(send to a messaging system eg: kafka) selectable emqttd message states:

This plugin in genral enhances the ability of a pub-sub system as it keeps on publishing messages or send to kafka on specific channels on various events like connect/disconnect/messageAck etc so that you can easily perform action on these events in any language just by having MQTT or kafka clients (example async task like storing these events in a database).


## Use cases: 
## publish messages on to

1> "SYSTEM/presence/connected" whene a client connects broker,

2> "SYSTEM/presence/disconnected" whene a client connects broker,

3> "SYSTEM/subscription/subscribed" when a client subscribes to a channel,

4> "SYSTEM/subscription/unsubscribed" when a client unsubscribes to a channel,

5> "SYSTEM/message" when a client publishes a message to a channel,

6> "SYSTEM/msgack" when a client acknowledges a messages (Very useful)


with a configurable QoS value

The data published on above events can be used to track message and client's activity in realtime.

## Details about message published on each channel:

## A> "SYSTEM/msgack"

A new message is published to "SYSTEM/msgack" whenever a client acknowledges a new message
the published payload is a following json string :

{
	"pub_client_id":"client id of who published this message",
	"ack_client_id":"client id of who acknowleded this message",
	"product_id":"your product id",
	"message_id":"your custom message id",
	"topic":"topic this message was published on"
}

All the above parameters can then be utilized to which clientId acknowledged which unique message published.

NOTE: you will  get acknowledgement on "SYSTEM/msgack" only if you publish your payload as serialized json with following mandatory keys:

{
	"product_id":"your custom product id",
	"message_id":"your custom message id",
	"payload":"data"
} 

why?
If you are interesting in getting the acknowledgement then while publishing the message you should specify the 
A = product_id and
B = message_id and make sure that A^B(A intersection B) is always unique.

Q.why product_id (isn't message id sufficient) 
Ans> this is because if you are using this in a company rhen there might exist a possibility that there are many products having same message id, therefore to uniquely identify a message you should send the message as a json with both product_id and message_id.


## B> "SYSTEM/presence/connected"

Whenever a client connects it publishes a message to this channel with payload (serialized json):

{
	"client_id":"which client id connected"
}

## C> "SYSTEM/presence/disconnected"

Whenever a client connects, it publishes a message to this channel with payload (serialized json):

{
	"client_id":"which client id disconnected"
}

## D> "SYSTEM/subscription/subscribed"
 
Whenever a client subscribes to a chennel, it publishes a message to this channel with payload (serialized json):

{
	"client_id":"which client id subscribed",
	"topic":"topic to which it subscribed"
}

## E> "SYSTEM/subscription/unsubscribed"

Whenever a client unsubscribes a channel, it publishes a message to this channel with payload (serialized json):

{
	"client_id":"which client id unsubscribed",
	"topic":"topic to which it unsubscribed"
}

## F> "SYSTEM/message"

Whenever a client publishes a new message on a channel with payload serialized json):

{
	"pub_client_id":"client id of publisher",
	"topic":"topic on which message is published",
	"payload":"message payload",
	"qos":2
} 




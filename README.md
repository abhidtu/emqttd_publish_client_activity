##emqttd_publish_client_activity

This is a work in progress relating to publishing messages on mqtt or Kafka topics relating to mqtt cliet's realtime activity using emqttd broker.

This plugin in genral enhances the ability of a pub-sub system as it keeps on publishing messages to mqtt or kafka topics on various events whenever a client connects/disconnects/publishes/acknowledges a message/subscribes to a topic/unsubscribe to a topic, so that you can easily perform action on these events in any programming language just by having MQTT or kafka clients in your project subscribed to below mentioned SYSTEM/# channels to get messages (example async tasks like storing these events in a database).


## Use cases: 
## publish messages on to

1> "SYSTEM/presence/connected" when a client connects broker,

2> "SYSTEM/presence/disconnected" when a client connects broker,

3> "SYSTEM/subscription/subscribed" when a client subscribes to a channel,

4> "SYSTEM/subscription/unsubscribed" when a client unsubscribes to a channel,

5> "SYSTEM/message" when a client publishes a message to a channel,

6> "SYSTEM/msgack" when a client acknowledges a messages (Very useful)


with a configurable QoS value (in progres)

The data published on above events can be used to track message and client's activity in realtime.

## Details about message published on each channel:

## A> "SYSTEM/msgack"

A new message is published to "SYSTEM/msgack" whenever a client acknowledges a new message
the published payload is a following json string :

{</br>
	"pub_client_id":"client id of who published this message",</br>
	"ack_client_id":"client id of who acknowleded this message",</br>
	"product_id":"your product id",</br>
	"message_id":"your custom message id",</br>
	"topic":"topic this message was published on"</br>
}

All the above parameters can then be utilized to figure out which clientId acknowledged which uniquely published message.

NOTE: you will  get acknowledgement on "SYSTEM/msgack" only if you publish your payload as serialized json with following mandatory keys:

{</br>
	"product_id":"your custom product id",</br>
	"message_id":"your custom message id",</br>
	"payload":"data"</br>
} 

why?</br>
If you are interesting in getting the acknowledgement then while publishing the message you should specify the </br>
A = product_id and</br>
B = message_id and make sure that A^B(A intersection B) is always unique.

Q.why product_id (isn't just message_id sufficient) </br>
Ans> this is because if you are using this in a company then there might exist a possibility that there are many products having same message_id, therefore to uniquely identify a message you should send the message as a json with both product_id and message_id.


## B> "SYSTEM/presence/connected"

Whenever a client connects it publishes a message to this channel with payload (serialized json):

{</br>
	"client_id":"which client id connected"</br>
}

## C> "SYSTEM/presence/disconnected"

Whenever a client disconnects it publishes a message to this channel with payload (serialized json):

{</br>
	"client_id":"which client id disconnected",</br>
	"reason":"reason why it got disconnected"</br>
}

## D> "SYSTEM/subscription/subscribed"
 
Whenever a client subscribes to a chennel, it publishes a message to this channel with payload (serialized json):
<pre>
{
	"client_id":"which client id subscribed",
	"topic":"topic to which it subscribed"
}
</pre>
## E> "SYSTEM/subscription/unsubscribed"

Whenever a client unsubscribes a channel, it publishes a message to this channel with payload (serialized json):

{</br>
	"client_id":"which client id unsubscribed",</br>
	"topic":"topic to which it unsubscribed"</br>
}

## F> "SYSTEM/message"

Whenever a client publishes a new message on a channel with payload serialized json):

{</br>
	"pub_client_id":"client id of publisher",</br>
	"topic":"topic on which message is published",</br>
	"payload":"message payload",</br>
	"qos":"qos with which message was sent"</br>
} 

##Goals:</br>
Integration with kafka, configurable option to publish these messages to kafka/mqtt SYSTEM/# topics

##Important:</br>
 make sure you dont have any client_id with "broker" since it is reserved for emqtt broker in this plugin.

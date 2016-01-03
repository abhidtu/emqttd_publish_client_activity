<pre>
This is a work in progress relating to publish/(send to a messaging system eg: kafka) selectable emqttd message states

Use cases: 
publish messages on to

1> "SYSTEM/presence/connected" whene a client connects broker,

2> "SYSTEM/presence/disconnected" whene a client connects broker,

3> "SYSTEM/subscription" when a client subscribes/unsubscribes to a channel,

4> "SYSTEM/message" when a client publishes a message to a channel,

5> "SYSTEM/msgack" when a client acknowledges a messages

with a configurable QoS value

The data published on above events can be used to track message and client's activity in realtime.

A new message is published to "SYSTEM/msgack" whenever a client acknowledges a new message
the published payload is a following json string :

{
	"pub_client_id":"client id of who published this message",
	"ack_client_id":"client id of who acknowleded this message",
	"product_id":"your product id",
	"message_id":"your custom message id",
	"topic":"topic this message was published on"
}

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
</pre>

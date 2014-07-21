# LevelDB message index store for RabbitMQ

This project implements a RabbitMQ _message store index_ using **eLevelDB** as backend database. Basically the message index keeps tracks of which messages are stored on which files on your filesystem. For more details on how RabbitMQ's message store work read the documentation on this file which is quite extensive [http://hg.rabbitmq.com/rabbitmq-server/file/59fa7d144fe1/src/rabbit_msg_store.erl](http://hg.rabbitmq.com/rabbitmq-server/file/59fa7d144fe1/src/rabbit_msg_store.erl). There's also a blog post from the RabbitMQ team here: [http://www.rabbitmq.com/blog/2011/01/20/rabbitmq-backing-stores-databases-and-disks/](http://www.rabbitmq.com/blog/2011/01/20/rabbitmq-backing-stores-databases-and-disks/).

## Installation

Get the `rabbitmq-public-umbrella`

		$ hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
		$ cd rabbitmq-public-umbrella
		$ make co

Get the [eleveldb_wrapper](https://github.com/cloudamqp/eleveldb_wrapper):

Inside the `rabbitmq-public-umbrella` directory do:

		$ git clone git://github.com/cloudamqp/eleveldb_wrapper.git

Then clone this repository:

		$ git clone git://github.com/cloudamqp/msg_store_eleveldb_index.git
		$ cd msg_store_eleveldb_index
		$ make

Copy the files inside `msg_store_eleveldb_index/dist` into your RabbitMQ `plugins` folder. Don't copy the file `rabbit_common-0.0.0.ez`.

eLevelDB required SMP, so on single core machiens add to `/etc/rabbitmq/rabbitmq-env.conf`:

    SERVER_START_ARGS="-smp enable"

Enable the plugin with:

    rabbitmq-plugins enable msg_store_eleveldb_index

To make RabbitMQ use the plugin as index module you have to configure it in `/etc/rabbitmq/rabbitmq.config`:

    {rabbit, [{msg_store_index_module, msg_store_eleveldb_index}]}

## Use case

In a default RabbitMQ installation your RAM amount will limit how many messages you can enqueue. This plugin allow you to queue messages until you run out of disc space.

## Creds

Based on works of [Alvaro Videla](https://github.com/videlalvaro).

Thanks to Basho for eLevelDB and to Google for LevelDB.


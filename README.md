# LevelDB Message Index for RabbitMQ

This project implements a RabbitMQ message index using [LevelDB](https://github.com/google/leveldb) for on-disk storage.

That eliminates the fixed per-message RAM cost RabbitMQ's default (RAM-only) message index has, allowing you to queue messages until you run out of disk space.

## Read Before Using This Module

When using this module, **make sure you have more file descriptors allocated to the RabbitMQ process**. RabbitMQ's
file descriptor monitor will not account for the descriptors used by LevelDB.

Failing to do so may result in RabbitMQ process running out of file descriptors and not noticing it. This **can lead
to data loss**.

We recommend allowing at least `65536` file descriptors per node when used with this plugin.

## Supported RabbitMQ Releases

This plugin targets RabbitMQ `3.4.4` and later versions.


## Installation

Prerequisites:

	apt-get install git-core build-essential xsltproc zip erlang-dev

Get the `rabbitmq-public-umbrella`

	git clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
	cd rabbitmq-public-umbrella
	make co

Get the [eleveldb_wrapper](https://github.com/cloudamqp/eleveldb_wrapper):

Inside the `rabbitmq-public-umbrella` directory do:

	git clone git://github.com/cloudamqp/eleveldb_wrapper.git

Then clone this repository:

	git clone git://github.com/cloudamqp/msg_store_eleveldb_index.git
	cd msg_store_eleveldb_index
	make

Copy the files inside `msg_store_eleveldb_index/dist` into your RabbitMQ `plugins` folder. Don't copy the file `rabbit_common-0.0.0.ez`.

	cp dist/eleveldb-*.ez dist/msg_store_eleveldb_index-*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-*/plugins/


Enable the plugin with:

    rabbitmq-plugins enable msg_store_eleveldb_index

To make RabbitMQ use the plugin as index module you have to configure it in `/etc/rabbitmq/rabbitmq.config`:

    {rabbit, [{msg_store_index_module, msg_store_eleveldb_index}]}

eLevelDB requires SMP. On single core machines please add the following to to `/etc/rabbitmq/rabbitmq-env.conf`:

    SERVER_START_ARGS="-smp enable"

## Credits

Based on the work of [Alvaro Videla](https://github.com/videlalvaro).

Thanks to Basho for eLevelDB and to Google for LevelDB.

## License

See [./LICENSE].

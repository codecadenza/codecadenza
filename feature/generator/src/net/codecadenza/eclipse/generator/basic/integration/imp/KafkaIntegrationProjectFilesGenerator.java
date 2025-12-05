/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.basic.integration.imp;

import static net.codecadenza.eclipse.shared.Constants.BASE_KAFKA_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for basic source and configuration files necessary for supporting Kafka
 * </p>
 * <p>
 * Copyright 2021 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class KafkaIntegrationProjectFilesGenerator extends AbstractIntegrationProjectFilesGenerator {
	private static final String KAFKA_FILE_CONSUMER = "FileKAFKAConsumer";

	/**
	 * Constructor
	 * @param module
	 * @param artifactType
	 */
	public KafkaIntegrationProjectFilesGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		super(module, artifactType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final var sourceFiles = new ArrayList<JavaFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_KAFKA) {
			final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;

			final var baseClientClass = new JavaFile(project, artifactType, BASE_KAFKA_CLIENT_CLASS_NAME, packageName);
			baseClientClass.setComment("Abstract base class for all Kafka clients");
			baseClientClass.setContent(createBaseClient());

			sourceFiles.add(baseClientClass);

			final var fileClientClass = new JavaFile(project, artifactType, module.getFileServiceClientName(), packageName);
			fileClientClass.setComment("Client for file operations via Kafka");
			fileClientClass.setContent(createFileServiceClient());

			sourceFiles.add(fileClientClass);

			if (module.isAddProducers())
				sourceFiles.add(createFileServiceProducer());
		}
		else if (artifactType == BuildArtifactType.INTEGRATION_IMP_KAFKA) {
			final String packageName = module.getNamespace().toString();

			final var fileConsumer = new JavaFile(project, artifactType, KAFKA_FILE_CONSUMER, packageName + SUB_PACKAGE_BEAN);
			fileConsumer.setComment("Kafka consumer for file operations");
			fileConsumer.setContent(createFileConsumer());

			sourceFiles.add(fileConsumer);
		}

		return sourceFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> createConfigurationFiles() {
		final var fileList = new ArrayList<WorkspaceFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_KAFKA) {
			final var path = project.getConfigFolder(artifactType) + "/kafka_config.properties";

			fileList.add(new WorkspaceFile(project, artifactType, path, createClientConfig()));
		}

		return fileList;
	}

	/**
	 * Create the base class for all Kafka clients
	 * @return the generated content
	 */
	private String createBaseClient() {
		final var b = new StringBuilder();
		b.append("import java.lang.invoke.*;\n");
		b.append("import java.math.*;\n");
		b.append("import java.nio.charset.*;\n");
		b.append("import java.time.*;\n");
		b.append("import java.time.temporal.*;\n");
		b.append("import java.util.*;\n");
		b.append("import java.util.concurrent.*;\n");
		b.append("import org.apache.avro.specific.*;\n");
		b.append("import org.apache.kafka.clients.consumer.*;\n");
		b.append("import org.apache.kafka.clients.producer.*;\n");
		b.append("import org.apache.kafka.common.*;\n");
		b.append("import org.apache.kafka.common.header.*;\n");
		b.append("import org.apache.kafka.common.header.internals.*;\n");
		b.append("import org.apache.kafka.common.serialization.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import net.codecadenza.runtime.avro.util.*;\n");
		b.append("import net.codecadenza.runtime.property.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n\n");
		b.append("public abstract class " + BASE_KAFKA_CLIENT_CLASS_NAME + " implements AutoCloseable\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("public static final String SCHEMA_KEY = \"SCHEMA\";\n");
		b.append("public static final String CORRELATION_ID_KEY = \"CORRELATION_ID\";\n");
		b.append("public static final String PARTITION_ID = \"PARTITION_ID\";\n");
		b.append("private static final String PROP_FILE_NAME = \"config/kafka_config.properties\";\n");
		b.append("private static final String PROP_BROKER_URL = \"broker.url\";\n");
		b.append("private static final String PROP_GROUP_ID = \"group.id\";\n");
		b.append("private static final String PROP_RESPONSE_TIMEOUT = \"response.timeout\";\n");
		b.append("private static final String PROP_OFFSET_TIMEOUT = \"response.offset.timeout\";\n");
		b.append("private static final String PROP_OFFSET_LOOK_BACK_SIZE = \"response.offset.lookback\";\n\n");
		b.append("protected final PropertyService propertyService = new PropertyService(PROP_FILE_NAME);\n");
		b.append("protected final Producer<String, byte[]> producer;\n");
		b.append("protected final KafkaConsumer<String, byte[]> consumer;\n");
		b.append("protected final Random random = new Random();\n");
		b.append("protected final Duration responseTimeout;\n");
		b.append("protected final Duration offsetTimeout;\n");
		b.append("protected final int lookBackSize;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("protected " + BASE_KAFKA_CLIENT_CLASS_NAME + "()\n");
		b.append("{\n");
		b.append("this.responseTimeout = Duration.ofMillis(propertyService.getLongProperty(PROP_RESPONSE_TIMEOUT));\n");
		b.append("this.offsetTimeout = Duration.ofMillis(propertyService.getLongProperty(PROP_OFFSET_TIMEOUT));\n");
		b.append("this.lookBackSize = propertyService.getIntProperty(PROP_OFFSET_LOOK_BACK_SIZE);\n");
		b.append("this.producer = getKafkaProducer();\n");
		b.append("this.consumer = getKafkaConsumer();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Send the given request object to the specified Kafka topic\n");
		b.append(" * @param topic the name of the topic\n");
		b.append(" * @param requestObject the request object that should be sent\n");
		b.append(" * @throws RemoteOperationException if the request message could not be sent\n");
		b.append(" * @throws AvroObjectSerializationException if the request object could not be serialized\n");
		b.append(" */\n");
		b.append("protected <T extends SpecificRecordBase> void sendRequest(String topic, T requestObject)\n");
		b.append("{\n");
		b.append("sendRequest(topic, requestObject, null, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Send the given request object to the specified Kafka topic\n");
		b.append(" * @param topic the name of the topic\n");
		b.append(" * @param requestObject the request object that should be sent\n");
		b.append(" * @param correlationId an optional correlation ID\n");
		b.append(" * @throws RemoteOperationException if the request message could not be sent\n");
		b.append(" * @throws AvroObjectSerializationException if the request object could not be serialized\n");
		b.append(" */\n");
		b.append("protected <T extends SpecificRecordBase> void sendRequest(String topic, T requestObject, UUID correlationId)\n");
		b.append("{\n");
		b.append("sendRequest(topic, requestObject, correlationId, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Send the given request object to the specified Kafka topic\n");
		b.append(" * @param topic the name of the topic\n");
		b.append(" * @param requestObject the request object that should be sent\n");
		b.append(" * @param correlationId an optional correlation ID\n");
		b.append(" * @param partitionId the optional partition ID that should be used\n");
		b.append(" * @throws RemoteOperationException if the request message could not be sent\n");
		b.append(" * @throws AvroObjectSerializationException if the request object could not be serialized\n");
		b.append(" */\n");
		b.append("protected <T extends SpecificRecordBase> void ");
		b.append("sendRequest(String topic, T requestObject, UUID correlationId, Integer partitionId)\n");
		b.append("{\n");
		b.append("final var schema = requestObject.getSchema().getName();\n");
		b.append("final var payload = new AvroObjectSerializer<>(requestObject).serialize();\n\n");
		b.append("final var producerRecord = new ProducerRecord<String, byte[]>(topic, payload);\n");
		b.append("producerRecord.headers().add(new RecordHeader(SCHEMA_KEY, schema.getBytes(StandardCharsets.UTF_8)));\n\n");
		b.append("if(partitionId != null)\n");
		b.append("{\n");
		b.append("logger.debug(\"Add partition ID '{}' to header\", partitionId);\n\n");
		b.append("producerRecord.headers().add(new RecordHeader(PARTITION_ID, BigInteger.valueOf(partitionId).toByteArray()));\n");
		b.append("}\n\n");
		b.append("if(correlationId != null)\n");
		b.append("{\n");
		b.append("logger.debug(\"Add correlation ID '{}' to header\", correlationId);\n\n");
		b.append("producerRecord.headers().add(new RecordHeader(CORRELATION_ID_KEY, ");
		b.append("correlationId.toString().getBytes(StandardCharsets.UTF_8)));\n");
		b.append("}\n\n");
		b.append("logger.debug(\"Send '{}' message to topic {}\", schema, topic);\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("// Block until the request message has been sent!\n");
		b.append("producer.send(producerRecord).get();\n\n");
		b.append("logger.debug(\"'{}' message sent to topic {}\", schema, topic);\n");
		b.append("}\n");
		b.append("catch (final InterruptedException e)\n");
		b.append("{\n");
		b.append("Thread.currentThread().interrupt();\n\n");
		b.append("if(partitionId == null)\n");
		b.append("logger.warn(\"Waiting for message to be sent to topic {} has been interrupted!\", topic);\n");
		b.append("else\n");
		b.append("logger.warn(\"Waiting for message to be sent to partition {} of topic {} has been interrupted!\", ");
		b.append("partitionId, topic);\n\n");
		b.append("throw new RemoteOperationException(\"Error while sending message to topic \" + topic);\n");
		b.append("}\n");
		b.append("catch (final ExecutionException e)\n");
		b.append("{\n");
		b.append("if(partitionId == null)\n");
		b.append("logger.warn(\"Error while sending message to topic {}!\", topic);\n");
		b.append("else\n");
		b.append("logger.warn(\"Error while sending message to partition {} of topic {}!\", partitionId, topic);\n\n");
		b.append("throw new RemoteOperationException(\"Error while sending message to topic \" + topic);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Get a response object with the expected correlation ID from the specified topic\n");
		b.append(" * @param <T> the expected Avro object type\n");
		b.append(" * @param topic the name of the response topic\n");
		b.append(" * @param responseObj an instance of the expected object that is ");
		b.append("used to determine the expected type and the Avro schema\n");
		b.append(" * @param correlationId the correlation ID\n");
		b.append(" * @return the response object\n");
		b.append(" * @throws RemoteOperationException");
		b.append("if the polling from the Kafka broker failed or the corresponding response message could not be found\n");
		b.append(" */\n");
		b.append("protected <T extends SpecificRecordBase> T getResponse(String topic, T responseObj, UUID correlationId)\n");
		b.append("{\n");
		b.append("return getResponse(topic, responseObj, correlationId, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Get a response object with the expected correlation ID from the specified topic\n");
		b.append(" * @param <T> the expected Avro object type\n");
		b.append(" * @param topic the name of the response topic\n");
		b.append(" * @param responseObj an instance of the expected object that is ");
		b.append("used to determine the expected type and the Avro schema\n");
		b.append(" * @param correlationId the correlation ID\n");
		b.append(" * @param partitionId the partition ID that the consumer expects to get the response from\n");
		b.append(" * @return the response object\n");
		b.append(" * @throws RemoteOperationException");
		b.append("if the polling from the Kafka broker failed or the corresponding response message could not be found\n");
		b.append(" */\n");
		b.append("protected <T extends SpecificRecordBase> T ");
		b.append("getResponse(String topic, T responseObj, UUID correlationId, Integer partitionId)\n");
		b.append("{\n");
		b.append("final AvroObjectDeserializer<T> deserializer = new AvroObjectDeserializer<>(responseObj.getSchema());\n");
		b.append("final LocalDateTime timeout = LocalDateTime.now().plus(responseTimeout.toMillis(), ChronoUnit.MILLIS);\n");
		b.append("final Set<TopicPartition> assignedPartitions;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(partitionId == null)\n");
		b.append("{\n");
		b.append("consumer.subscribe(Collections.singleton(topic));\n");
		b.append("assignedPartitions = waitForAssignment(timeout);\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("assignedPartitions = Set.of(new TopicPartition(topic, partitionId));\n");
		b.append("consumer.assign(assignedPartitions);\n");
		b.append("}\n\n");
		b.append("// Seek to the proper start offset (look‑back)\n");
		b.append("prepareOffsets(assignedPartitions);\n\n");
		b.append("// Main polling loop – stop when either the message has been found or when a timeout has occurred\n");
		b.append("while(LocalDateTime.now().isBefore(timeout))\n");
		b.append("{\n");
		b.append("final ConsumerRecords<String, byte[]> records = consumer.poll(Duration.ofMillis(50));\n\n");
		b.append("for(final ConsumerRecord<String, byte[]> consumerRecord : records)\n");
		b.append("{\n");
		b.append("if(!hasCorrelationId(consumerRecord, correlationId))\n");
		b.append("{\n");
		b.append("// Not the message we are looking for!\n");
		b.append("continue;\n");
		b.append("}\n\n");
		b.append("final T payload = deserializer.deserialize(consumerRecord.value());\n");
		b.append("final var topicPartition = new TopicPartition(consumerRecord.topic(), consumerRecord.partition());\n\n");
		b.append("// Commit the next offset\n");
		b.append("consumer.commitSync(Map.of(topicPartition, new OffsetAndMetadata(consumerRecord.offset() + 1)));\n\n");
		b.append("return payload;\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("throw new TimeoutException(");
		b.append("\"Response with correlation ID \" + correlationId + \" not received within \"");
		b.append(" + responseTimeout.toMillis() + \" milliseconds!\");\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new RemoteOperationException(\"Error while waiting for response! Reason:\" + e.getMessage());\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("// Always leave the consumer in a known state\n");
		b.append("try\n");
		b.append("{\n");
		b.append("consumer.unsubscribe();\n");
		b.append("consumer.assign(Collections.emptyList());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("logger.warn(\"Error while resetting Kafka consumer!\", e);\n");
		b.append("}\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Prepare the offsets to consume messages that have already been processed\n");
		b.append(" * @param partitions the partitions to set the start offset\n");
		b.append(" */\n");
		b.append("private void prepareOffsets(Set<TopicPartition> partitions)\n");
		b.append("{\n");
		b.append("final Map<TopicPartition, Long> endOffsets = consumer.endOffsets(partitions, offsetTimeout);\n\n");
		b.append("for(final Map.Entry<TopicPartition, Long> entry : endOffsets.entrySet())\n");
		b.append("{\n");
		b.append("final long start = (entry.getValue() > lookBackSize) ? entry.getValue() - lookBackSize : 0L;\n");
		b.append("consumer.seek(entry.getKey(), start);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Block until the consumer receives a non‑empty partition assignment or the timeout expires\n");
		b.append(" * @param timeout the point in time at which the method must stop waiting for the assignment\n");
		b.append(" */\n");
		b.append("private Set<TopicPartition> waitForAssignment(LocalDateTime timeout) \n");
		b.append("{\n");
		b.append("while(LocalDateTime.now().isBefore(timeout))\n");
		b.append("{\n");
		b.append("consumer.poll(Duration.ofMillis(50));\n\n");
		b.append("final Set<TopicPartition> assignedPartitions = consumer.assignment();\n\n");
		b.append("if(!assignedPartitions.isEmpty())\n");
		b.append("return assignedPartitions;\n");
		b.append("}\n\n");
		b.append("throw new RemoteOperationException(\"Failed to obtain partition assignment within \"");
		b.append(" + responseTimeout.toMillis() + \" milliseconds!\");\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Check if the given consumer record contains the expected correlation ID\n");
		b.append(" * @param consumerRecord the consumer record\n");
		b.append(" * @param correlationId the expected correlation ID\n");
		b.append(" * @return true if the correlation ID has been found\n");
		b.append(" */\n");
		b.append("protected boolean hasCorrelationId(ConsumerRecord<String, byte[]> consumerRecord, UUID correlationId)\n");
		b.append("{\n");
		b.append("for(Header header : consumerRecord.headers().headers(CORRELATION_ID_KEY))\n");
		b.append("{\n");
		b.append("final var responseId = UUID.fromString(new String(header.value(), StandardCharsets.UTF_8));\n\n");
		b.append("if(responseId.equals(correlationId))\n");
		b.append("{\n");
		b.append("logger.debug(\"Received response message with expected correlation ID '{}'\", responseId);\n");
		b.append("return true;\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("return false;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Get any partition of the given topic\n");
		b.append(" * @param topic the name of the topic\n");
		b.append(" * @return a partition ID\n");
		b.append(" */\n");
		b.append("protected int getRandomPartition(String topic)\n");
		b.append("{\n");
		b.append("final var partitionList = producer.partitionsFor(topic);\n\n");
		b.append("// Pick a random partition\n");
		b.append("final var randomIndex = random.nextInt(partitionList.size());\n\n");
		b.append("return partitionList.get(randomIndex).partition();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a consumer based on the configuration properties that ");
		b.append("are loaded from the respective configuration file\n");
		b.append(" * @return a new Kafka consumer\n");
		b.append(" */\n");
		b.append("protected KafkaConsumer<String, byte[]> getKafkaConsumer()\n");
		b.append("{\n");
		b.append("final var props = new Properties();\n");
		b.append("props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, propertyService.getStringProperty(PROP_BROKER_URL));\n");
		b.append("props.put(ConsumerConfig.GROUP_ID_CONFIG, propertyService.getStringProperty(PROP_GROUP_ID));\n");
		b.append("props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());\n");
		b.append("props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, ByteArrayDeserializer.class.getName());\n");
		b.append("props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, Boolean.FALSE.toString());\n\n");
		b.append("return new KafkaConsumer<>(props);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a producer based on the configuration properties that ");
		b.append("are loaded from the respective configuration file\n");
		b.append(" * @return a new Kafka consumer\n");
		b.append(" */\n");
		b.append("protected Producer<String, byte[]> getKafkaProducer()\n");
		b.append("{\n");
		b.append("final var props = new Properties();\n");
		b.append("props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, propertyService.getStringProperty(PROP_BROKER_URL));\n");
		b.append("props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());\n");
		b.append("props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, ByteArraySerializer.class.getName());\n\n");
		b.append("return new KafkaProducer<>(props);\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see java.lang.AutoCloseable#close()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void close()\n");
		b.append("{\n");
		b.append("producer.close();\n");
		b.append("consumer.close();\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the client for handling files
	 * @return the generated content
	 */
	private String createFileServiceClient() {
		final var b = new StringBuilder();
		b.append("import java.io.*;\n");
		b.append("import java.lang.invoke.*;\n");
		b.append("import java.nio.*;\n");
		b.append("import java.nio.channels.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import java.util.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import net.codecadenza.runtime.avro.file.*;\n");
		b.append("import net.codecadenza.runtime.avro.response.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n");
		b.append("import net.codecadenza.runtime.transport.file.*;\n\n");
		b.append("public class " + module.getFileServiceClientName() + " extends " + BASE_KAFKA_CLIENT_CLASS_NAME);
		b.append(" implements " + module.getFileServiceName() + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String REQUEST_TOPIC = \"file-handling-request\";\n");
		b.append("private static final String RESPONSE_TOPIC = \"file-handling-response\";\n");
		b.append("private static final int BUFFER_SIZE = 1 << 18;\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#uploadFile(java.io.File)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String uploadFile(File file)\n");
		b.append("{\n");
		b.append("final var partitionId = getRandomPartition(RESPONSE_TOPIC);\n");
		b.append("String path = null;\n\n");
		b.append("final var request = new UploadFileRequest();\n");
		b.append("request.setFileName(file.getName());\n\n");
		b.append("logger.info(\"Upload file {}\", file.getName());\n\n");
		b.append("try(final FileChannel fileChannel = FileChannel.open(file.toPath(), StandardOpenOption.READ))\n");
		b.append("{\n");
		b.append("int chunkIndex = 0;\n");
		b.append("long size = fileChannel.size();\n\n");
		b.append("while(size > 0)\n");
		b.append("{\n");
		b.append("final var correlationId = UUID.randomUUID();\n");
		b.append("final ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);\n");
		b.append("final int bytesRead = fileChannel.read(buffer);\n\n");
		b.append("if(bytesRead <= 0)\n");
		b.append("break;\n\n");
		b.append("buffer.flip();\n");
		b.append("size -= bytesRead;\n\n");
		b.append("logger.debug(\"Upload chunk {} with {} byte(s) of file {}\", chunkIndex, bytesRead, file.getName());\n\n");
		b.append("request.setContent(buffer);\n");
		b.append("request.setPath(path);\n\n");
		b.append("sendRequest(REQUEST_TOPIC, request, correlationId, partitionId);\n\n");
		b.append("// Upload a single chunk\n");
		b.append("final var response = getResponse(RESPONSE_TOPIC, new UploadFileResponse(), correlationId, partitionId);\n\n");
		b.append("if(chunkIndex == 0)\n");
		b.append("path = response.getPath();\n\n");
		b.append("chunkIndex++;\n\n");
		b.append("if(response.getResponseStatus().getCode() != ResponseCode.SUCCESS)\n");
		b.append("throw new RemoteOperationException(response.getResponseStatus().getMessage());\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while uploading file {}!\", file.getName(), e);\n\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n\n");
		b.append("logger.info(\"Finished upload of file {}. The remote storage path is {}\", file.getName(), path);\n\n");
		b.append("return path;\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#downloadFile(java.lang.String, java.io.File)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void downloadFile(String pathOnServer, File targetFile)\n");
		b.append("{\n");
		b.append("final var request = new DownloadFileRequest();\n");
		b.append("request.setPath(pathOnServer);\n");
		b.append("request.setSize(BUFFER_SIZE);\n");
		b.append("request.setStartIndex(0);\n\n");
		b.append("logger.info(\"Download file {}\", targetFile.getName());\n\n");
		b.append("try(FileOutputStream output = new FileOutputStream(targetFile, true))\n");
		b.append("{\n");
		b.append("while(true)\n");
		b.append("{\n");
		b.append("final var correlationId = UUID.randomUUID();\n");
		b.append("final var partitionId = getRandomPartition(RESPONSE_TOPIC);\n\n");
		b.append("logger.debug(\"Request content for file {} from start position {} \"");
		b.append(", targetFile.getName(), request.getStartIndex());\n\n");
		b.append("sendRequest(REQUEST_TOPIC, request, correlationId, partitionId);\n\n");
		b.append("// Request one file chunk\n");
		b.append("final var response = getResponse(RESPONSE_TOPIC, new DowloadFileResponse(), correlationId, partitionId);\n\n");
		b.append("if(response.getResponseStatus().getCode() == ResponseCode.ERROR)\n");
		b.append("throw new RemoteOperationException(response.getResponseStatus().getMessage());\n\n");
		b.append("logger.debug(\"Received {} byte(s) for file {}\", response.getContent().limit(), targetFile.getName());\n\n");
		b.append("// Append the target file with the full content of every chunk\n");
		b.append("output.write(response.getContent().array());\n\n");
		b.append("// Exit the loop as soon as the returned content doesn't fully fill the internal buffer\n");
		b.append("if(response.getContent().array().length < BUFFER_SIZE)\n");
		b.append("break;\n\n");
		b.append("request.setStartIndex(request.getStartIndex() + BUFFER_SIZE);\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while downloading file {}!\", targetFile.getName(), e);\n\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n\n");
		b.append("logger.info(\"Finished download of file {}\", targetFile.getName());\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the kafka_config.properties file for the client artifact
	 * @return the generated content
	 */
	private String createClientConfig() {
		final var b = new StringBuilder();
		b.append("broker.url=localhost:9092\n");
		b.append("group.id=" + project.getCode().toLowerCase() + "-group\n");
		b.append("response.timeout=6000\n");
		b.append("response.offset.timeout=2000\n");
		b.append("response.offset.lookback=10\n");

		return b.toString();
	}

	/**
	 * Create the Kafka file consumer
	 * @return the generated content
	 */
	private String createFileConsumer() {
		final var b = new StringBuilder();
		b.append("import static net.codecadenza.runtime.spring.kafka.KafkaHeaderHelper.getCorrelationIdFromHeader;\n");
		b.append("import static net.codecadenza.runtime.spring.kafka.KafkaHeaderHelper.getRequestSchemaFromHeader;\n");
		b.append("import static net.codecadenza.runtime.spring.kafka.KafkaHeaderHelper.getResponsePartitionIdFromHeader;\n");
		b.append("import java.io.*;\n");
		b.append("import java.lang.invoke.*;\n");
		b.append("import java.nio.*;\n");
		b.append("import java.nio.channels.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import org.apache.kafka.clients.consumer.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import org.springframework.kafka.annotation.*;\n");
		b.append("import org.springframework.stereotype.*;\n");
		b.append("import net.codecadenza.runtime.avro.file.*;\n");
		b.append("import net.codecadenza.runtime.avro.response.*;\n");
		b.append("import net.codecadenza.runtime.avro.util.*;\n");
		b.append("import net.codecadenza.runtime.property.*;\n");
		b.append("import net.codecadenza.runtime.spring.kafka.*;\n\n");
		b.append("@Component\n");
		b.append("public class " + KAFKA_FILE_CONSUMER + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String PATH_EXCHANGE_FOLDER = ");
		b.append("new PropertyService().getStringProperty(PropertyService.PROP_EXCHANGE_FOLDER);\n");
		b.append("private static final String REQUEST_TOPIC = \"file-handling-request\";\n");
		b.append("private static final String RESPONSE_TOPIC = \"file-handling-response\";\n");
		b.append("private static final String GROUP_ID = \"file-handling-group\";\n\n");
		b.append("private final KafkaSender kafkaSender;\n\n");
		b.append("/**\n");
		b.append(" * Constructor for injecting the Kafka sender\n");
		b.append(" * @param kafkaSender\n");
		b.append(" */\n");
		b.append("public " + KAFKA_FILE_CONSUMER + "(KafkaSender kafkaSender)\n");
		b.append("{\n");
		b.append("this.kafkaSender = kafkaSender;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Listen for incoming messages from the respective topic\n");
		b.append(" * @param consumerRecord the incoming consumer record\n");
		b.append(" */\n");
		b.append("@KafkaListener(groupId = GROUP_ID, topics = REQUEST_TOPIC)\n");
		b.append("public void onMessageReceived(ConsumerRecord<String, byte[]> consumerRecord)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var payload = consumerRecord.value();\n");
		b.append("final var schema = getRequestSchemaFromHeader(consumerRecord);\n\n");
		b.append("if(payload != null)\n");
		b.append("logger.info(\"Received {} byte(s) with schema {}\", payload.length, schema);\n\n");
		b.append("// Perform an operation depending on the provided request schema\n");
		b.append("if(schema.equals(UploadFileRequest.getClassSchema().getName()))\n");
		b.append("handleUploadMessage(payload, getCorrelationIdFromHeader(consumerRecord),");
		b.append("getResponsePartitionIdFromHeader(consumerRecord));\n");
		b.append("else if(schema.equals(DownloadFileRequest.getClassSchema().getName()))\n");
		b.append("handleDownloadMessage(payload, getCorrelationIdFromHeader(consumerRecord),");
		b.append("getResponsePartitionIdFromHeader(consumerRecord));\n");
		b.append("else\n");
		b.append("throw new IllegalStateException(\"The schema '\" + schema + \"' is not supported!\");\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("// Only log the exception so that the listener can commit the message offset\n");
		b.append("logger.error(\"Error while processing request message!\", e);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Save the content of a file\n");
		b.append(" * @param payload a byte array that contains the message payload\n");
		b.append(" * @param correlationId the correlation ID provided by the corresponding request message\n");
		b.append(" * @param partitionId the ID of the partition where the response should be saved at\n");
		b.append(" * @throws AvroObjectDeserializationException if the payload could not be deserialized\n");
		b.append(" * @throws AvroObjectSerializationException if the response object could not be serialized\n");
		b.append(" * @throws KafkaSenderException if the response message could not be sent\n");
		b.append(" */\n");
		b.append("protected void handleUploadMessage(byte[] payload, byte[] correlationId, int partitionId)\n");
		b.append("{\n");
		b.append("final var schema = UploadFileRequest.getClassSchema();\n");
		b.append("final var request = new AvroObjectDeserializer<UploadFileRequest>(schema).deserialize(payload);\n");
		b.append("final var responseStatus = ResponseStatusInitializer.withSuccessStatus();\n");
		b.append("final var response = UploadFileResponse.newBuilder().setResponseStatus(responseStatus).build();\n");
		b.append("final File targetFile;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(request.getPath() != null)\n");
		b.append("{\n");
		b.append("// For larger files that are filled by multiple requests the message ");
		b.append("must contain the actual path of the target file!\n");
		b.append("targetFile = new File(request.getPath());\n\n");
		b.append("if(!Files.exists(targetFile.toPath()))\n");
		b.append("{\n");
		b.append("final var notFoundMessage = \"The file '\" + targetFile.getName() + \"' could not be found!\";\n\n");
		b.append("response.setResponseStatus(ResponseStatusInitializer.with(ResponseCode.NOT_FOUND, notFoundMessage));\n\n");
		b.append("kafkaSender.sendResponse(RESPONSE_TOPIC, response, correlationId, partitionId);\n");
		b.append("return;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("// Create an initial target file if the message doesn't contain the respective path\n");
		b.append("if(!PATH_EXCHANGE_FOLDER.isEmpty())\n");
		b.append("{\n");
		b.append("// Create the target file in the exchange folder\n");
		b.append("targetFile = new File(PATH_EXCHANGE_FOLDER + request.getFileName() + System.currentTimeMillis());\n\n");
		b.append("Files.createFile(targetFile.toPath());\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("// Create a temporary file where the content should be saved to\n");
		b.append("targetFile = Files.createTempFile(request.getFileName(), Long.toString(System.currentTimeMillis())).toFile();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("logger.info(\"Write {} byte(s) to {}\", request.getContent().array().length, request.getPath());\n\n");
		b.append("Files.write(targetFile.toPath(), request.getContent().array(), StandardOpenOption.APPEND);\n\n");
		b.append("response.setPath(targetFile.getAbsolutePath());\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while uploading file '\" + request.getFileName() + \"'!\", e);\n\n");
		b.append("response.setResponseStatus(ResponseStatusInitializer.fromException(e));\n");
		b.append("}\n\n");
		b.append("kafkaSender.sendResponse(RESPONSE_TOPIC, response, correlationId, partitionId);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Download a file\n");
		b.append(" * @param payload a byte array that contains the message payload\n");
		b.append(" * @param correlationId the correlation ID provided by the corresponding request message\n");
		b.append(" * @param partitionId the ID of the partition where the response should be saved at\n");
		b.append(" * @throws AvroObjectDeserializationException if the payload could not be deserialized\n");
		b.append(" * @throws AvroObjectSerializationException if the response object could not be serialized\n");
		b.append(" * @throws KafkaSenderException if the response message could not be sent\n");
		b.append(" */\n");
		b.append("protected void handleDownloadMessage(byte[] payload, byte[] correlationId, int partitionId)\n");
		b.append("{\n");
		b.append("final var schema = DownloadFileRequest.getClassSchema();\n");
		b.append("final var request = new AvroObjectDeserializer<DownloadFileRequest>(schema).deserialize(payload);\n");
		b.append("final var responseStatus = ResponseStatus.newBuilder().setCode(ResponseCode.SUCCESS).build();\n");
		b.append("final var response = DowloadFileResponse.newBuilder().setResponseStatus(responseStatus).build();\n\n");
		b.append("try(final FileChannel fileChannel = FileChannel.open(");
		b.append("new File(request.getPath()).toPath(), StandardOpenOption.READ))\n");
		b.append("{\n");
		b.append("final int size = (int) Math.min(fileChannel.size(), request.getSize());\n");
		b.append("final var buffer = ByteBuffer.allocate(size);\n");
		b.append("final var bytesRead = fileChannel.read(buffer, request.getStartIndex());\n\n");
		b.append("if(bytesRead > 0)\n");
		b.append("{\n");
		b.append("logger.info(\"Read {} byte(s) from {}\", bytesRead, request.getPath());\n\n");
		b.append("response.setContent(buffer);\n");
		b.append("buffer.flip();\n");
		b.append("}\n");
		b.append("else\n");
		b.append("response.setContent(ByteBuffer.allocate(0));\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while downloading file!\", e);\n\n");
		b.append("response.setResponseStatus(ResponseStatusInitializer.fromException(e));\n");
		b.append("}\n\n");
		b.append("kafkaSender.sendResponse(RESPONSE_TOPIC, response, correlationId, partitionId);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

}

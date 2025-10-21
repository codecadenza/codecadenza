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
package net.codecadenza.eclipse.generator.integration.bean.kafka;

import static net.codecadenza.eclipse.model.java.JavaType.CHAR;
import static net.codecadenza.eclipse.model.java.JavaType.CHARACTER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.common.AbstractContentFormatter;
import net.codecadenza.eclipse.generator.integration.method.imp.kafka.BasicKafkaMethodGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for Avro IDL files
 * </p>
 * <p>
 * Copyright 2021 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class KafkaIntegrationIDLGenerator extends AbstractContentFormatter {
	private static final String COMMON_AVRO_NAMESPACE = "net.codecadenza.runtime.avro";
	private static final String ARRAY = "array";
	private static final String ENUM = "enum";
	private static final String RECORD = "record";
	private static final String RESPONSE_STATUS_TYPE = COMMON_AVRO_NAMESPACE + ".response.ResponseStatus";
	private static final String RESPONSE_STATUS = RESPONSE_STATUS_TYPE + " responseStatus;";
	private static final String TYPE_DATE = "date";
	private static final String TYPE_DECIMAL = "decimal";
	private static final String TYPE_INT = "int";
	private static final String TYPE_STRING = "string";
	private static final String TYPE_UUID = COMMON_AVRO_NAMESPACE + ".types.Uuid";
	private static final String TYPE_TIMESTAMP = "timestamp_ms";
	private static final String SEARCH_INPUT = COMMON_AVRO_NAMESPACE + ".search.SearchInput searchInput;";
	private static final String UNION = "union";

	private final AbstractIntegrationBean integrationBean;
	private final Project project;
	private final DomainObject domainObject;
	private final List<KafkaIntegrationMethod> kafkaIntegrationMethods;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public KafkaIntegrationIDLGenerator(AbstractIntegrationBean integrationBean) {
		this.integrationBean = integrationBean;
		this.domainObject = integrationBean.getDomainObject();
		this.project = domainObject.getNamespace().getProject();
		this.kafkaIntegrationMethods = integrationBean.getMethods().stream().map(KafkaIntegrationMethod.class::cast).toList();
	}

	/**
	 * Create the Avro interface definition file for the given integration bean
	 * @throws Exception if the file could not be created in the workspace
	 */
	public void createSourceFile() throws Exception {
		final String domainObjectName = domainObject.getName().toLowerCase();
		final var path = project.getResourceFolder() + "/" + domainObjectName.toLowerCase() + ".avdl";

		EclipseIDEService
				.createOrUpdateFile(new WorkspaceFile(project, BuildArtifactType.INTEGRATION_SEI_KAFKA, path, createContent()));
	}

	/**
	 * @return the generated content
	 */
	protected String createContent() {
		final var exchangeElements = new HashSet<DataExchangeElement>();

		addBlockComment("Interface that contains data structures for processing " + domainObject.getLabel() + " objects");
		addLine("@namespace(\"" + integrationBean.getNamespace().toString() + SUB_PACKAGE_INT_AVRO + "\")");
		addLine("protocol " + domainObject.getName());
		addLine("{");

		increaseIndent();

		// Collect all data transfer objects that are required by this interface
		final var dtoSet = kafkaIntegrationMethods.stream().map(this::collectDataTransferObjects).flatMap(Set::stream)
				.collect(Collectors.toSet());

		final boolean importResponseSchema = kafkaIntegrationMethods.stream().anyMatch(KafkaIntegrationMethod::isSendResponse);

		final boolean importSearchInputSchema = kafkaIntegrationMethods.stream().map(KafkaIntegrationMethod::getBoundaryMethod)
				.anyMatch(method -> method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH
						|| method.getMethodType() == BoundaryMethodTypeEnumeration.COUNT);

		// Search for all data exchange elements with mapping objects that must be added to the interface
		for (final var integrationMethod : kafkaIntegrationMethods) {
			final BoundaryMethodTypeEnumeration methodType = integrationMethod.getBoundaryMethod().getMethodType();

			if (methodType == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT
					|| methodType == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT) {
				final var exchangeMethod = (DataExchangeMethod) integrationMethod.getBoundaryMethod().getServiceMethod();

				if (exchangeMethod.getExchangeMode() instanceof DirectExchangeMode) {
					if (exchangeMethod.getJoinedImportMethod() != null)
						exchangeElements.addAll(exchangeMethod.getJoinedImportMethod().getRootElement().getAllElements());
					else
						exchangeElements.addAll(exchangeMethod.getRootElement().getAllElements());
				}
			}
		}

		// Import all required standard IDL files
		if (importResponseSchema)
			addLine("import idl \"ResponseStatus.avdl\";");

		if (importSearchInputSchema)
			addLine("import idl \"SearchInput.avdl\";");

		addLine("import idl \"CustomTypes.avdl\";");

		collectDTOEnums(dtoSet).forEach(this::addEnum);

		collectMappingObjectEnums(exchangeElements).forEach(this::addEnum);

		// Create a recordset for every required data transfer object
		dtoSet.forEach(this::addDTO);

		// Create a recordset for every mapping object
		exchangeElements.stream().filter(element -> element.getMappingObject() != null).map(DataExchangeElement::getMappingObject)
				.collect(Collectors.toSet()).forEach(this::addMappingObject);

		// Create all request operations
		kafkaIntegrationMethods.forEach(this::addRequest);

		// Create all response operations
		kafkaIntegrationMethods.forEach(this::addResponse);

		decreaseIndent();

		addLine("}");

		return getContent();
	}

	/**
	 * Add a recordset based on the provided data transfer object to the interface
	 * @param dto
	 */
	private void addDTO(DTOBean dto) {
		addBlankLine();
		addBlockComment(dto.getComment());
		addLine(RECORD + " " + dto.getName());
		addLine("{");

		increaseIndent();

		for (final DTOBeanAttribute attr : dto.getAttributes()) {
			boolean optional = false;
			boolean isArray = false;
			String dataTypeName;
			var type = "";

			if (attr.getDomainAttribute() != null) {
				final var javaType = attr.getDomainAttribute().getJavaType();
				dataTypeName = getAvroTypeName(javaType, attr.getModifier(), attr.getDomainAttribute().getColumn());
				isArray = attr.getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE;

				if (attr.getDomainAttribute().isPersistent())
					optional = attr.getDomainAttribute().isSetDateOnPersist()
							|| attr.getDomainAttribute().getDomainAttributeValidator().isNullable();
				else
					optional = !attr.getDomainAttribute().getJavaType().isPrimitive();
			}
			else {
				dataTypeName = getAvroTypeName(attr.getReferencedDTOBean(), attr.getModifier(), null);

				if (attr.getAssociation() instanceof final OneToOneAssociation oto)
					optional = oto.isOptional();
				else if (attr.getAssociation() instanceof final ManyToOneAssociation mto)
					optional = mto.isOptional();
				else
					isArray = true;
			}

			if (isArray)
				type = ARRAY + "<" + dataTypeName + ">";
			else
				type = dataTypeName;

			type += optional ? "?" : "";

			addLine(type + " " + attr.getName() + ";");
		}

		decreaseIndent();

		addLine("}");
	}

	/**
	 * Add a recordset based on the provided mapping object to the interface
	 * @param mappingObject
	 */
	private void addMappingObject(ExchangeMappingObject mappingObject) {
		addBlankLine();
		addBlockComment(mappingObject.getComment());
		addLine(RECORD + " " + mappingObject.getName());
		addLine("{");

		increaseIndent();

		mappingObject.getAttributes().forEach(attr -> {
			final JavaType javaType = attr.getJavaType();
			String dataTypeName = getAvroTypeName(javaType, attr.getModifier(), null);
			boolean optional = false;
			boolean isArray = false;
			var type = "";

			if (attr.getDomainAttribute() != null) {
				if (attr.getDomainAttribute().isPersistent())
					optional = attr.getDomainAttribute().isSetDateOnPersist()
							|| attr.getDomainAttribute().getDomainAttributeValidator().isNullable();
				else
					optional = !attr.getJavaType().isPrimitive();

				isArray = attr.getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE;
				dataTypeName = getAvroTypeName(javaType, attr.getModifier(), attr.getDomainAttribute().getColumn());
			}
			else if (attr.getAssociation() != null) {
				if (attr.getAssociation() instanceof final OneToOneAssociation oto)
					optional = oto.isOptional();
				else if (attr.getAssociation() instanceof final ManyToOneAssociation mto)
					optional = mto.isOptional();
			}
			else if (attr.getModifier() == JavaTypeModifierEnumeration.NONE) {
				// At this point it is hard to determine if the field should either be optional or mandatory. Thus, every non-primitive
				// field is marked as optional!
				optional = !attr.getJavaType().isPrimitive();
			}

			if (isArray)
				type = ARRAY + "<" + dataTypeName + ">";
			else
				type = dataTypeName;

			type += optional ? "?" : "";

			addLine(type + " " + attr.getName() + ";");
		});

		decreaseIndent();

		addLine("}");
	}

	/**
	 * Add the given enumeration to the interface
	 * @param enumeration
	 */
	private void addEnum(JavaEnum enumeration) {
		addBlankLine();

		if (enumeration.getComment() != null && !enumeration.getComment().isEmpty())
			addBlockComment(enumeration.getComment());

		addLine(ENUM + " " + enumeration.getName());
		addLine("{");

		increaseIndent();

		addLine(enumeration.getEnumerationValues().stream().map(EnumLiteral::getName).collect(Collectors.joining(", ")));

		decreaseIndent();

		addLine("}");
	}

	/**
	 * Add a request recordset for the given method
	 * @param method
	 */
	private void addRequest(KafkaIntegrationMethod method) {
		addBlankLine();
		addBlockComment("Request schema for \"" + method.getName() + "\" operations");
		addLine(RECORD + " " + method.getRequestSchemaName());
		addLine("{");

		increaseIndent();

		final BoundaryMethodTypeEnumeration methodType = method.getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.SEARCH || methodType == BoundaryMethodTypeEnumeration.COUNT)
			addLine(SEARCH_INPUT);

		method.getMethodParameters()
				.forEach(param -> addLine(getAvroTypeName(param.getType(), param.getModifier(), null) + " " + param.getName() + ";"));

		for (final IntegrationMethodParameter param : method.getIntegrationParameters()) {
			if (method.getMethodParameters().stream().map(MethodParameter::getName).anyMatch(name -> name.equals(param.getName())))
				continue;

			final var javaType = project.getJavaTypeByName(param.getType());

			if (javaType != null)
				addLine(getAvroTypeName(javaType, JavaTypeModifierEnumeration.NONE, null) + " " + param.getName() + ";");
		}

		decreaseIndent();

		addLine("}");
	}

	/**
	 * Add a response recordset if the given method provides a respective schema
	 * @param method
	 */
	private void addResponse(KafkaIntegrationMethod method) {
		if (method.getResponseSchemaName() == null || method.getResponseSchemaName().isEmpty())
			return;

		addBlankLine();
		addBlockComment("Response schema for \"" + method.getName() + "\" operations");
		addLine(RECORD + " " + method.getResponseSchemaName());
		addLine("{");

		increaseIndent();

		addLine(RESPONSE_STATUS);

		if (!method.getReturnType().isVoid()) {
			final String avroTypeName = getAvroTypeName(method.getReturnType(), method.getReturnTypeModifier(), null);

			addLine(UNION + " {null, " + avroTypeName + "} responseData = null;");
		}

		for (final var entrySet : BasicKafkaMethodGenerator.getAdditionalResponseFields(method).entrySet()) {
			final String avroTypeName = getAvroTypeName(entrySet.getValue(), JavaTypeModifierEnumeration.NONE, null);

			addLine(UNION + " {null, " + avroTypeName + "} " + entrySet.getKey() + " = null;");
		}

		decreaseIndent();

		addLine("}");
	}

	/**
	 * Get the Avro type based on the provided {@link JavaType}
	 * @param javaType
	 * @param modifier
	 * @param column
	 * @return the Avro type name
	 */
	private String getAvroTypeName(JavaType javaType, JavaTypeModifierEnumeration modifier, DBColumn column) {
		final String avroTypeName;

		if (javaType.isLong() || javaType.isDouble() || javaType.isFloat() || javaType.isBoolean() || javaType.isString())
			avroTypeName = javaType.getName().toLowerCase();
		else if (javaType.isInteger())
			avroTypeName = TYPE_INT;
		else if (javaType.isUUID())
			avroTypeName = TYPE_UUID;
		else if (javaType.isType(CHAR, CHARACTER) || javaType.isByteArray())
			avroTypeName = TYPE_STRING;
		else if (javaType.isDateOrCalendar() || javaType.isLocalDateTime())
			avroTypeName = TYPE_TIMESTAMP;
		else if (javaType.isLocalDate())
			avroTypeName = TYPE_DATE;
		else if (javaType.isBigDecimal()) {
			// Default values for the scale and the precision must be added if a BigDecimal attribute isn't mapped to a database column!
			final int precision = column != null && column.getPrecision() != 0 ? column.getPrecision() : 10;
			final int scale = column != null && column.getScale() != 0 ? column.getScale() : 4;

			avroTypeName = TYPE_DECIMAL + "(" + precision + ", " + scale + ")";
		}
		else
			avroTypeName = javaType.getName();

		// Only arrays are supported!
		if (modifier != JavaTypeModifierEnumeration.NONE)
			return ARRAY + "<" + avroTypeName + ">";

		return avroTypeName;
	}

	/**
	 * @param method
	 * @return all data transfer objects of the given method
	 */
	private Set<DTOBean> collectDataTransferObjects(KafkaIntegrationMethod method) {
		final var dtoSet = new HashSet<DTOBean>();

		if (method.getReturnType() instanceof final DTOBean dto)
			dtoSet.add(dto);

		dtoSet.addAll(method.getMethodParameters().stream().map(MethodParameter::getType).filter(DTOBean.class::isInstance)
				.map(DTOBean.class::cast).toList());

		// Search for all data transfer objects that are used by DTO attributes
		final var attributeDTOs = dtoSet.stream().flatMap(dto -> dto.getAttributes().stream())
				.filter(attr -> attr.getReferencedDTOBean() != null).map(DTOBeanAttribute::getReferencedDTOBean)
				.collect(Collectors.toSet());

		dtoSet.addAll(attributeDTOs);

		return dtoSet;
	}

	/**
	 * @param dtoList
	 * @return all required enumerations
	 */
	private Set<JavaEnum> collectDTOEnums(Set<DTOBean> dtoList) {
		return dtoList.stream().flatMap(dto -> dto.getAttributes().stream()).filter(attr -> attr.getDomainAttribute() != null)
				.map(DTOBeanAttribute::getDomainAttribute).map(DomainAttribute::getJavaType).filter(JavaEnum.class::isInstance)
				.map(JavaEnum.class::cast).collect(Collectors.toSet());
	}

	/**
	 * @param exchangeElements
	 * @return all required enumerations
	 */
	private Set<JavaEnum> collectMappingObjectEnums(Set<DataExchangeElement> exchangeElements) {
		return exchangeElements.stream().filter(element -> element.getMappingObject() != null)
				.map(DataExchangeElement::getMappingObject).flatMap(obj -> obj.getAttributes().stream())
				.map(ExchangeMappingAttribute::getJavaType).filter(JavaEnum.class::isInstance).map(JavaEnum.class::cast)
				.collect(Collectors.toSet());
	}

	/**
	 * Put the given text into a block comment
	 * @param text
	 */
	public void addBlockComment(String text) {
		if (text == null)
			return;

		addLine("/**");

		final String[] lines = text.split("\n");

		for (String line : lines) {
			line = line.trim();

			if (!line.isEmpty())
				addLine(" * " + line);
		}

		addLine(" */");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractContentFormatter#getDefaultIndent()
	 */
	@Override
	public String getDefaultIndent() {
		return "\t";
	}

}

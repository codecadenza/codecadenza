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
package net.codecadenza.eclipse.generator.exchange.method;

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Abstract base class for data export method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractExportMethodGenerator extends AbstractExchangeMethodGenerator {
	protected static final String EXPORT_METHOD_PREFIX = "export";
	protected static final String LIST_ITEM_SUFFIX = "ListItem";
	protected static final String DATA_EXPORT_LIST = "dataExportObjList";

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	protected AbstractExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);
	}

	/**
	 * @param marshalledObject
	 * @return the generated content
	 */
	protected abstract String addMarshallerInvocation(String marshalledObject);

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createAdditionalMethods()
	 */
	@Override
	public void createAdditionalMethods() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethod()
	 */
	@Override
	public void createMethod() {
		final var b = new StringBuilder();
		final String objectLabel = method.getDataExchangeServiceBean().getDomainObject().getLabel();

		b.append(createMethodComment());
		b.append(addAnnotations());

		if (project.isSpringBootApplication())
			b.append("@Transactional\n");

		b.append("public " + getMethodSignature(true) + "\n");
		b.append("{\n");
		b.append(createInvocationDelayFragment());
		b.append("try\n");
		b.append("{\n");

		if (generator != null) {
			if (method.isProcessSingleObject())
				generator.addDebugLog(b, "Export " + objectLabel);
			else
				generator.addDebugLog(b, "Export " + objectLabel + " objects");

			b.append("\n");
		}

		if (method.isProcessSingleObject())
			b.append(createMethodForSingleObject());
		else
			b.append(createMethodForMultipleObjects());

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new DataExportException(e, true);\n");
		b.append("}\n");
		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(getMethodSignature(true), b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethodComment()
	 */
	@Override
	public String createMethodComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));

		if (method.getReturnType().isString()) {
			if (!method.returnsPath())
				b.append(" * @return the generated content string\n");
			else
				b.append(" * @return the absolute path of the generated export file\n");
		}
		else if (!method.getReturnType().isVoid())
			b.append(" * @return the generated export object\n");

		b.append(" * @throws DataExportException if export operation has failed\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#addImports()
	 */
	@Override
	public void addImports() {
		super.addImports();

		if (generator == null)
			return;

		if (!method.isProcessSingleObject())
			generator.importPackage(PACK_JAVA_UTIL);

		if (project.isSpringBootApplication())
			generator.importPackage("org.springframework.transaction.annotation");

		generator.importPackage(exchangeBean.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethodForSingleObject()
	 */
	@Override
	public String createMethodForSingleObject() {
		final var b = new StringBuilder();
		final String domainObjectName = method.getDataExchangeServiceBean().getDomainObject().getName();
		var paramName = "";

		for (final MethodParameter param : method.getMethodParameters()) {
			if (!(param instanceof final FilterMethodParameter filterParam))
				continue;

			paramName = filterParam.getName();
		}

		if (stringExchangeMode)
			b.append("final var outputWriter = new StringWriter();\n");

		final RepositoryMethod finderMethod = method.getDataExchangeServiceBean().getDomainObject()
				.findRepositoryMethod(RepositoryMethodTypeEnumeration.FIND_BY_ID);

		if (finderMethod == null) {
			b.append(createOutputForMissingRepositoryMethod(method.getDataExchangeServiceBean().getDomainObject(),
					RepositoryMethodTypeEnumeration.FIND_BY_ID));

			if (!method.getReturnType().isVoid())
				b.append("\nreturn null;\n");

			return b.toString();
		}

		final Repository repository = finderMethod.getRepository();
		final String repositoryName = createRepositoryName(repository.getDomainObject());

		repositories.put(repositoryName, repository);

		b.append("final " + domainObjectName + " " + DEFAULT_DOMAIN_OBJECT_NAME + " = " + repositoryName);
		b.append("." + finderMethod.getName() + "(" + paramName + ");\n\n");
		b.append("if(" + DEFAULT_DOMAIN_OBJECT_NAME + " == null)\n");
		b.append("throw new IllegalStateException(\"Object with ID '\" + " + paramName + " + \"' not found!\");\n\n");
		b.append(exportDataExchangeElement(method.getRootElement(true), DEFAULT_DOMAIN_OBJECT_NAME, null, true));

		if (fileExchangeMode != null) {
			b.append(createFileName());
			b.append("final var outputFile = new File(\"" + fileExchangeMode.getPath() + "\" + fileName);\n");
		}
		else
			b.append("\n");

		if (directExchangeMode)
			b.append("return " + method.getRootElement(true).getMappingObject().getDomainObject().getLowerCaseName() + ";\n");
		else
			b.append(addMarshallerInvocation(method.getDataExchangeServiceBean().getDomainObject().getLowerCaseName()));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethodForMultipleObjects()
	 */
	@Override
	public String createMethodForMultipleObjects() {
		final var b = new StringBuilder();
		final String generatedStatement = method.getQueryStatement().replace(System.lineSeparator(), " ").trim();
		final String customStatement = method.getCustomStatement().replace(System.lineSeparator(), " ").trim();
		String statement;

		if (!customStatement.isEmpty())
			statement = generatedStatement + " " + customStatement;
		else
			statement = generatedStatement;

		final String rootMappingClassName = method.getRootElement(true).getMappingObject().getName();

		b.append("final var queryStatement = \"" + statement + "\";\n");
		b.append("final var parameterMap = new HashMap<String, Object>();\n");

		if (method.getContentType() != ContentTypeEnumeration.XML)
			b.append("final var " + DATA_EXPORT_LIST + " = new ArrayList<" + rootMappingClassName + ">();\n");
		else
			b.append("final var " + DATA_EXPORT_LIST + " = new " + rootMappingClassName + "();\n");

		if (stringExchangeMode)
			b.append("final var outputWriter = new StringWriter();\n");

		if (fileExchangeMode != null) {
			if (fileExchangeMode.getBlockSize() == null) {
				b.append(createFileName());
				b.append("final var outputFile = new File(\"" + fileExchangeMode.getPath() + "\" + fileName);\n");
			}
			else {
				b.append("final int maxBatchSize = " + fileExchangeMode.getBlockSize() + ";\n");
				b.append("int " + BATCH_COUNTER + " = 0;\n");
				b.append("int " + BATCH_INDEX + " = 0;\n");
				b.append("int exportCounter = 0;\n");
			}
		}

		b.append("\n");

		// Set all filter parameters
		for (final MethodParameter param : method.getMethodParameters())
			if (param instanceof FilterMethodParameter)
				b.append("parameterMap.put(\"" + param.getName() + "\", " + param.getName() + ");\n");

		b.append("\n");

		final RepositoryMethod searchMethod = method.getDataExchangeServiceBean().getDomainObject()
				.findRepositoryMethod(RepositoryMethodTypeEnumeration.SEARCH);

		if (searchMethod == null) {
			b.append(createOutputForMissingRepositoryMethod(method.getDataExchangeServiceBean().getDomainObject(),
					RepositoryMethodTypeEnumeration.SEARCH));

			if (!method.getReturnType().isVoid())
				b.append("\nreturn null;\n");

			return b.toString();
		}

		final Repository repository = searchMethod.getRepository();
		final String repositoryName = createRepositoryName(repository.getDomainObject());

		repositories.put(repositoryName, repository);

		b.append("final List<" + exchangeBean.getDomainObject().getName() + "> " + DEFAULT_OBJECT_LIST_NAME + " = ");
		b.append(repositoryName + "." + searchMethod.getName() + "(queryStatement, ");

		if (method.getExchangeMode().getMaxObjectsToBeProcessed() == null)
			b.append("Integer.MAX_VALUE");
		else
			b.append(method.getExchangeMode().getMaxObjectsToBeProcessed());

		b.append(", 0, parameterMap);\n\n");
		b.append("// Iterate over all objects in result list\n");
		b.append("for(final " + exchangeBean.getDomainObject().getName() + " ");
		b.append(DEFAULT_DOMAIN_OBJECT_NAME + " : " + DEFAULT_OBJECT_LIST_NAME + ")\n");
		b.append("{\n");

		if (fileExchangeMode != null && fileExchangeMode.getBlockSize() != null) {
			b.append(BATCH_COUNTER + "++;\n");
			b.append("exportCounter++;\n\n");
		}

		if (method.getContentType() == ContentTypeEnumeration.XML)
			method.getRootElement(true).getSubElements()
					.forEach(element -> b.append(exportDataExchangeElement(element, DEFAULT_DOMAIN_OBJECT_NAME, DATA_EXPORT_LIST, true)));
		else
			b.append(exportDataExchangeElement(method.getRootElement(true), DEFAULT_DOMAIN_OBJECT_NAME, DATA_EXPORT_LIST, false));

		b.append("}\n\n");

		if (directExchangeMode)
			b.append("return " + DATA_EXPORT_LIST + ";\n");
		else if (fileExchangeMode == null || fileExchangeMode.getBlockSize() == null)
			b.append(addMarshallerInvocation(DATA_EXPORT_LIST));

		return b.toString();
	}

	/**
	 * @param attr
	 * @param targetMappingObjectName
	 * @param sourceDomainObjectName
	 * @param getter
	 * @param setter
	 * @return the generated content
	 */
	protected String createMapping(ExchangeMappingAttribute attr, String targetMappingObjectName, String sourceDomainObjectName,
			String getter, String setter) {
		final var b = new StringBuilder();
		final DomainAttribute domainAttr = attr.getDomainAttribute();

		b.append(targetMappingObjectName + "." + setter + "(");

		if (generator != null && domainAttr != null && domainAttr.getCollectionType() != CollectionTypeEnumeration.NONE)
			generator.importPackage("java.util.stream");

		// By definition, we handle fields of type char by using a String with exactly one character!
		if (attr.getJavaType().isChar() && (domainAttr == null || domainAttr.getCollectionType() == CollectionTypeEnumeration.NONE))
			b.append("String.valueOf(");

		b.append(sourceDomainObjectName + "." + getter);

		if (domainAttr != null && domainAttr.getCollectionType() != CollectionTypeEnumeration.NONE) {
			if (generator != null)
				generator.importPackage("java.util");

			b.append(".stream().");

			if (attr.getJavaType().isChar())
				b.append("map(Object::toString).");

			if (domainAttr.getCollectionType() == CollectionTypeEnumeration.SET)
				b.append("collect(Collectors.toCollection(HashSet::new))");
			else if (domainAttr.getCollectionType() == CollectionTypeEnumeration.LIST)
				b.append("collect(Collectors.toCollection(ArrayList::new))");
		}

		if (attr.getJavaType().isChar() && (domainAttr == null || domainAttr.getCollectionType() == CollectionTypeEnumeration.NONE))
			b.append(")");

		b.append(");\n");

		return b.toString();
	}

	/**
	 * @param attr
	 * @param targetMappingObjectName
	 * @param sourceDomainObjectName
	 * @return the generated content
	 */
	protected String createAttributeExportMapping(ExchangeMappingAttribute attr, String targetMappingObjectName,
			String sourceDomainObjectName) {
		final var b = new StringBuilder();
		final DomainAttribute domainAttribute = attr.getDomainAttribute();

		if (attr.getAssociation() == null) {
			final String setter = attr.getSetterName();
			final String getter = domainAttribute.getGetterName();

			b.append(createMapping(attr, targetMappingObjectName, sourceDomainObjectName, getter, setter));
		}
		else {
			// In this mode we have to take care of deeply cascaded association lists in order to prevent NullPointerExceptions when
			// accessing optional associations!
			final String nullCheck = attr.getNullCheck(sourceDomainObjectName, null);
			final String getter = attr.getAssociationListGetter() + attr.getAssociation().getGetterName() + "."
					+ domainAttribute.getGetterName();

			if (!nullCheck.isEmpty())
				b.append("\n");

			b.append(nullCheck);
			b.append(createMapping(attr, targetMappingObjectName, sourceDomainObjectName, getter, attr.getSetterName()));

			if (!nullCheck.isEmpty())
				b.append("\n");
		}

		return b.toString();
	}

	/**
	 * @param element
	 * @param parentSourceObjectName
	 * @param targetParentObjectName
	 * @param usesMappingAssoc
	 * @return the generated content
	 */
	protected String exportDataExchangeElement(DataExchangeElement element, String parentSourceObjectName,
			String targetParentObjectName, boolean usesMappingAssoc) {
		final var b = new StringBuilder();
		String mappingAssocName = null;
		AbstractDomainAssociation assoc = null;
		String sourceObjectName = parentSourceObjectName;
		final ExchangeMappingObject mappingObject = element.getMappingObject();
		final ExchangeMappingAttribute mappingAttr = element.getMappingAttribute();

		if (element.isDisableExternalMapping() || mappingObject == null)
			return b.toString();

		if (usesMappingAssoc) {
			if (mappingAttr != null && mappingAttr.getAssociation() != null) {
				assoc = mappingAttr.getAssociation();

				for (final ExchangeMappingAttribute attr : element.getParentElement().getMappingObject().getAttributes())
					if (attr.getAssociation() != null && attr.getAssociation().equals(assoc))
						mappingAssocName = attr.getGetterName();
			}
			else if (element.getParentElement() != null)
				for (final ExchangeMappingAttribute attr : element.getParentElement().getMappingObject().getAttributes())
					mappingAssocName = attr.getGetterName();
			else
				for (final ExchangeMappingAttribute attr : mappingObject.getAttributes())
					mappingAssocName = attr.getGetterName();
		}

		String targetObjectName = mappingObject.getDomainObject().getLowerCaseName();

		if (assoc != null && (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation))
			targetObjectName = assoc.getName();

		if (!element.isContainer())
			return b.toString();

		final boolean exportList = assoc != null && (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation);

		if (exportList) {
			sourceObjectName = assoc.getTarget().getLowerCaseName() + LIST_ITEM_SUFFIX;
			final String getter = mappingAttr.getAssociationListGetter() + assoc.getGetterName();

			if (generator != null)
				generator.importPackage(mappingObject.getDomainObject().getNamespace().toString());

			b.append("\nfor(final " + mappingObject.getDomainObject().getName() + " ");
			b.append(sourceObjectName + " : " + parentSourceObjectName + "." + getter + ")\n");
			b.append("{\n");
		}

		if (!exportList && assoc != null)
			b.append("\n");

		b.append("final var " + targetObjectName + " = new " + mappingObject.getName() + "();\n");

		// Generate the mappings for all attributes of this element
		for (final DataExchangeElement subElement : element.getSubElements()) {
			if (subElement.isDisableExternalMapping() || subElement.isContainer())
				continue;

			if (subElement.getMappingAttribute() == null || subElement.getMappingAttribute().getDomainAttribute() == null)
				continue;

			b.append(createAttributeExportMapping(subElement.getMappingAttribute(), targetObjectName, sourceObjectName));
		}

		for (final DataExchangeAttribute attribute : element.getAttributes()) {
			final ExchangeMappingAttribute attr = attribute.getMappingAttribute();

			if (attribute.isDisableExternalMapping() || attr == null)
				continue;

			if (attr.getDomainAttribute() != null)
				b.append(createAttributeExportMapping(attr, targetObjectName, sourceObjectName));
			else if (attr.isJoinAttribute() && assoc != null) {
				// If data is exported it could be the case that a join column exists. Respective attributes should be filled!
				final DomainAttribute pkAttr = assoc.getDomainObject().getPKAttribute();

				b.append(targetObjectName + "." + attr.getSetterName() + "(");
				b.append(parentSourceObjectName + "." + pkAttr.getGetterName() + ");\n");
			}
		}

		if (exportList) {
			// The name of the source object is the name of the parent source object for the next level below!
			parentSourceObjectName = sourceObjectName;
		}

		for (final DataExchangeElement subElement : element.getSubElements()) {
			if (!subElement.isContainer())
				continue;

			if (subElement.getMappingAttribute() == null || subElement.getMappingAttribute().getAssociation() == null)
				continue;

			b.append(exportDataExchangeElement(subElement, parentSourceObjectName, targetObjectName, true));
		}

		if (targetParentObjectName != null) {
			b.append("\n" + targetParentObjectName + ".");

			if (usesMappingAssoc) {
				if (exportList || assoc == null)
					b.append(mappingAssocName + ".add(" + targetObjectName + ");\n");
				else
					b.append(assoc.getSetterName() + "(" + targetObjectName + ");\n");
			}
			else
				b.append("add(" + targetObjectName + ");\n");
		}

		if (exportList)
			b.append("}\n");

		if (assoc == null && targetParentObjectName != null)
			b.append(addFileBatchFragment(targetParentObjectName, mappingAssocName));

		return b.toString();
	}

	/**
	 * @param targetParentObjectName
	 * @param mappingAssocGetterName
	 * @return the generated content
	 */
	protected String addFileBatchFragment(String targetParentObjectName, String mappingAssocGetterName) {
		final var b = new StringBuilder();

		if (fileExchangeMode == null || fileExchangeMode.getBlockSize() == null)
			return b.toString();

		b.append("\nif(" + BATCH_COUNTER + " == maxBatchSize || exportCounter == " + DEFAULT_OBJECT_LIST_NAME + ".size())\n");
		b.append("{\n");
		b.append(BATCH_INDEX + "++;\n");
		b.append(createFileName());
		b.append("final var outputFile = new File(\"" + fileExchangeMode.getPath() + "\" + " + FILE_NAME + ");\n\n");
		b.append(addMarshallerInvocation(targetParentObjectName));
		b.append("\n" + BATCH_COUNTER + " = 0;\n");
		b.append(targetParentObjectName + ".");

		if (mappingAssocGetterName != null)
			b.append(mappingAssocGetterName + ".");

		b.append("clear();\n");
		b.append("}\n");

		return b.toString();
	}

}

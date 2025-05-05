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
package net.codecadenza.eclipse.generator.exchange;

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for data exchange mapping objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExchangeMappingObjectGenerator extends AbstractJavaSourceGenerator {
	private final ExchangeMappingObject mappingObject;
	private boolean addXMLBinding;
	private boolean addJSonBinding;

	/**
	 * Constructor
	 * @param mappingObject
	 */
	public ExchangeMappingObjectGenerator(ExchangeMappingObject mappingObject) {
		super(mappingObject.getSourceFile());

		this.mappingObject = mappingObject;

		final DataExchangeElement element = mappingObject.getDataExchangeElement();

		if (element != null) {
			if (element.getRootElement().getDataExchangeMethod().getContentType() == ContentTypeEnumeration.XML)
				this.addXMLBinding = true;
			else if (element.getRootElement().getDataExchangeMethod().getContentType() == ContentTypeEnumeration.JSON)
				this.addJSonBinding = true;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("java.io");

		if (addXMLBinding)
			importPackage("jakarta.xml.bind.annotation");

		mappingObject.getAttributes().forEach(attr -> {
			if (attr.getJavaType().getNamespace() != null && !attr.getJavaType().getNamespace().equals(mappingObject.getNamespace()))
				importPackage(attr.getJavaType().getNamespace().toString());

			if (attr.getModifier() != JavaTypeModifierEnumeration.NONE || (attr.getDomainAttribute() != null
					&& attr.getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE))
				importPackage(PACK_JAVA_UTIL);
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (addXMLBinding)
			b.append(addJAXBAnnotations());

		b.append("public class " + mappingObject.getName() + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		mappingObject.getAttributes().forEach(attr -> {
			final JavaType type = attr.getJavaType();
			final JavaTypeModifierEnumeration modifier = attr.getModifier();
			final String typeName = getTypeName(attr);
			JavaFieldGenerator fieldGenerator;

			if (modifier == JavaTypeModifierEnumeration.NONE) {
				fieldGenerator = addPrivateField(typeName, attr.getName());

				if (attr.getDefaultValue() != null && !attr.getDefaultValue().isEmpty()) {
					var defaultValue = "";

					if (type.isEnum())
						defaultValue = typeName + "." + attr.getDefaultValue();
					else if (type.isString() || type.isChar())
						defaultValue = "\"" + attr.getDefaultValue() + "\"";
					else if (type.isBoolean())
						defaultValue = "true";
					else if (type.isInteger())
						defaultValue = attr.getDefaultValue();
					else if (type.isLong())
						defaultValue = attr.getDefaultValue() + "L";
					else if (type.isUUID())
						defaultValue = "UUID.randomUUID()";
					else if (type.isFloat())
						defaultValue = attr.getDefaultValue() + "F";
					else if (type.isDouble())
						defaultValue = attr.getDefaultValue();
					else if (type.isBigDecimal())
						defaultValue = "new BigDecimal(\"" + attr.getDefaultValue() + "\")";
					else if (type.isDate())
						defaultValue = "new Date()";
					else if (type.isCalendar())
						defaultValue = "new GregorianCalendar()";
					else if (type.isLocalDate())
						defaultValue = "LocalDate.now()";
					else if (type.isLocalDateTime())
						defaultValue = "LocalDateTime.now()";

					fieldGenerator.withDefaultValue(defaultValue);
				}
			}
			else {
				fieldGenerator = addPrivateField(modifier.toString() + "<" + typeName + ">", attr.getName());
				fieldGenerator.withDefaultValue("new " + JavaTypeModifierEnumeration.ARRAY_LIST.toString() + "<>()");
			}

			if (addXMLBinding)
				fieldGenerator.withAnnotations(addJAXBAnnotations(attr));
			else if (addJSonBinding)
				fieldGenerator.withAnnotations(addJSONBAnnotations(attr));

			fieldGenerator.create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		// We don't need a default constructor if no other constructor must be added to this class!
		if (mappingObject.getPKAttribute() != null || mappingObject.getDisplayAttribute() != null)
			addConstructor(Collections.emptyList(), "Default constructor");

		// Add a constructor with the primary key attribute
		if (mappingObject.getPKAttribute() != null)
			addConstructor(Collections.singletonList(mappingObject.getPKAttribute()), "Constructor with ID attribute");

		// Add a constructor with the display attribute
		if (mappingObject.getDisplayAttribute() != null) {
			final var attrList = new ArrayList<ExchangeMappingAttribute>();

			if (mappingObject.getPKAttribute() != null)
				attrList.add(mappingObject.getPKAttribute());

			attrList.add(mappingObject.getDisplayAttribute());

			addConstructor(attrList, "Constructor with display attribute");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		StringBuilder b;

		// Add getters and setters
		for (final ExchangeMappingAttribute attr : mappingObject.getAttributes()) {
			final var comment = new StringBuilder();
			final JavaTypeModifierEnumeration modifier = attr.getModifier();
			final String typeName = getTypeName(attr);

			if (modifier == JavaTypeModifierEnumeration.NONE) {
				if (attr.getDomainAttribute() != null) {
					if (typeName.equalsIgnoreCase(JavaType.BOOL)) {
						if (attr.getAssociation() == null)
							comment.append("/**\n * @return true if " + attr.getDomainAttribute().getLabel() + " is set\n */\n");
						else {
							comment.append("/**\n * @return true if " + attr.getAssociation().getTarget().getLabel() + " ");
							comment.append(attr.getDomainAttribute().getLabel() + " is set\n */\n");
						}
					}
					else if (attr.getAssociation() == null)
						comment.append("/**\n * @return the " + attr.getDomainAttribute().getLabel() + "\n */\n");
					else {
						comment.append("/**\n * @return the " + attr.getAssociation().getTarget().getLabel() + " ");
						comment.append(attr.getDomainAttribute().getLabel() + "\n */\n");
					}
				}
				else
					comment.append("/**\n * @return the value of field " + attr.getName() + "\n */\n");

				b = new StringBuilder();
				b.append(comment.toString());
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + typeName + " " + attr.getGetterName() + "\n{\nreturn this." + attr.getName() + ";\n}\n\n");

				addMethod(typeName + " " + attr.getGetterName(), b.toString());

				b = new StringBuilder();

				if (attr.getDomainAttribute() != null)
					b.append("/**\n * @param " + attr.getName() + " the " + attr.getDomainAttribute().getLabel() + " to set.\n */\n");
				else
					b.append("/**\n * @param " + attr.getName() + "\n */\n");

				b.append(getAnnotationForGeneratedElement());
				b.append("public void " + attr.getSetterName() + "(" + typeName + " " + attr.getName());
				b.append(")\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

				addMethod("void " + attr.getSetterName() + "(" + typeName + " " + attr.getName() + ")", b.toString());
			}
			else {
				comment.append("/**\n * @return a list of elements\n */\n");

				b = new StringBuilder();
				b.append(comment.toString());
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + modifier.toString() + "<" + typeName + "> " + attr.getGetterName());
				b.append("\n{\nreturn this." + attr.getName() + ";\n}\n\n");

				addMethod(modifier.toString() + "<" + typeName + "> " + attr.getGetterName(), b.toString());

				b = new StringBuilder();
				b.append("/**\n * @param " + attr.getName() + "\n */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public void " + attr.getSetterName() + "(" + modifier.toString() + "<" + typeName + "> ");
				b.append(attr.getName() + ")\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

				addMethod("void " + attr.getSetterName() + "(" + modifier.toString() + "<" + typeName + "> " + attr.getName() + ")",
						b.toString());
			}
		}

		if (mappingObject.getPKAttribute() != null) {
			final ExchangeMappingAttribute pkAttribute = mappingObject.getPKAttribute();
			final String pkGetter = pkAttribute.getGetterName();
			final JavaType pkType = pkAttribute.getDomainAttribute().getJavaType();
			final ExchangeMappingAttribute displayAttr = mappingObject.getDisplayAttribute();

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.lang.Object#equals(java.lang.Object)\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("@Override\n");
			b.append("public boolean equals(Object obj)\n{\n");
			b.append("if(this == obj)\nreturn true;\n\n");
			b.append("if(obj == null)\nreturn false;\n\n");
			b.append("if(getClass() != obj.getClass())\nreturn false;\n\n");
			b.append("final var mappingObject = (" + mappingObject.getName() + ") obj;\n\n");

			if (!pkType.isPrimitive()) {
				b.append("if(this." + pkAttribute.getName() + " == null)\n");
				b.append("{\n");
				b.append("if(mappingObject." + pkGetter + " != null)\n");
				b.append("return false;\n");
				b.append("}\n");
				b.append("else if(!this." + pkAttribute.getName() + ".equals(mappingObject." + pkGetter + ")");

				if (displayAttr != null)
					b.append(" && !this." + displayAttr.getName() + ".equals(mappingObject." + displayAttr.getGetterName() + ")");

				b.append(")\n");
				b.append("return false;\n\n");
				b.append("return true;\n");
			}
			else {
				b.append("return this." + pkAttribute.getName() + " == mappingObject." + pkGetter);

				if (displayAttr != null)
					b.append(" && this." + displayAttr.getName() + ".equals(mappingObject." + displayAttr.getGetterName() + ")");

				b.append(";\n");
			}

			b.append("}\n\n");

			addMethod("boolean equals(Object obj)", b.toString());

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.lang.Object#hashCode()\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("@Override\n");
			b.append("public int hashCode()\n{\n");

			if (pkAttribute.getDomainAttribute().getJavaType().isPrimitive()) {
				if (pkType.isType(JavaType.INT))
					b.append("return " + pkAttribute.getName() + ";\n");
				else
					b.append("return (int) (" + pkAttribute.getName() + " ^ (" + pkAttribute.getName() + " >>> 32));\n");
			}
			else
				b.append("return (" + pkAttribute.getName() + " == null) ? 0 : " + pkAttribute.getName() + ".hashCode();\n");

			b.append("}\n\n");

			addMethod("int hashCode()", b.toString());
		}
	}

	/**
	 * Add a constructor
	 * @param attrList
	 * @param comment
	 * @return the generated content
	 */
	private String addConstructor(List<ExchangeMappingAttribute> attrList, String comment) {
		final var b = new StringBuilder();
		final var allParameters = new StringBuilder();
		final var allAttributes = new StringBuilder();
		final var allCommentParameters = new StringBuilder();
		boolean firstParam = true;

		for (final ExchangeMappingAttribute attr : attrList) {
			final String typeName = attr.getJavaType().getName();

			if (firstParam)
				firstParam = false;
			else
				allParameters.append(", ");

			allParameters.append(typeName);
			allParameters.append(" " + attr.getName());

			allAttributes.append("this." + attr.getName() + " = " + attr.getName() + ";\n");
			allCommentParameters.append(" * @param " + attr.getName() + "\n");
		}

		b.append("/**\n * " + comment + "\n" + allCommentParameters + " */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + mappingObject.getName() + "(" + allParameters + ")\n{\n" + allAttributes + "}\n\n");

		addConstructor(mappingObject.getName() + "(" + allParameters + ")", b.toString());

		return b.toString();
	}

	/**
	 * @return the JAXB annotations for a given mapping object
	 */
	private String addJAXBAnnotations() {
		final var b = new StringBuilder();
		final DataExchangeElement element = mappingObject.getDataExchangeElement();

		if (element == null)
			return b.toString();

		final DataExchangeElement rootElement = element.getRootElement();
		final Project project = rootElement.getDataExchangeMethod().getDataExchangeServiceBean().getNamespace().getProject();
		final var prefix = project.getXmlNamespacePrefix() != null ? project.getXmlNamespacePrefix() : "";

		if (element.equals(rootElement)) {
			b.append("@XmlRootElement(name=\"" + element.getName() + "\"");

			if (!prefix.isEmpty())
				b.append(", namespace=\"" + project.getXmlNamespace() + "\"");

			b.append(")\n");
		}

		b.append("@XmlType(name=\"" + element.getTypeName() + "\"");

		if (!element.equals(rootElement) && !prefix.isEmpty())
			b.append(", namespace=\"" + project.getXmlNamespace() + "\"");

		b.append(")\n");
		b.append("@XmlAccessorType(XmlAccessType.FIELD)\n");

		return b.toString();
	}

	/**
	 * @param mappingAttribute
	 * @return the JAXB annotations for a given mapping attribute
	 */
	private String addJAXBAnnotations(ExchangeMappingAttribute mappingAttribute) {
		final var b = new StringBuilder();
		var name = "";
		boolean required = true;
		boolean addBlank = false;
		boolean isTransient = false;
		final DataExchangeElement exchangeElement = mappingAttribute.getDataExchangeElement(true);
		final DataExchangeAttribute exchangeAttribute = mappingAttribute.getDataExchangeAttribute();

		if (exchangeElement != null) {
			if (!exchangeElement.isDisableExternalMapping()) {
				if (exchangeElement.getWrapperElementName() != null && !exchangeElement.getWrapperElementName().isEmpty())
					b.append("@XmlElementWrapper(name=\"" + exchangeElement.getWrapperElementName() + "\")\n");

				b.append("@XmlElement(");

				name = exchangeElement.getName();

				if (exchangeElement.getMinOccurrences() == null
						|| (exchangeElement.getMinOccurrences() != null && exchangeElement.getMinOccurrences() == 0))
					required = false;
			}
			else
				isTransient = true;
		}

		if (exchangeAttribute != null) {
			if (!exchangeAttribute.isDisableExternalMapping()) {
				b.append("@XmlAttribute(");

				name = exchangeAttribute.getName();
				required = !exchangeAttribute.isOptional();
			}
			else
				isTransient = true;
		}

		if (exchangeAttribute == null && exchangeElement == null)
			isTransient = true;

		if (!isTransient) {
			if (!mappingAttribute.getName().equals(name)) {
				addBlank = true;

				b.append("name=\"" + name + "\"");
			}

			if (addBlank)
				b.append(", ");

			if (required)
				b.append("required=true");
			else
				b.append("required=false");

			b.append(")\n");

			if (mappingAttribute.getJavaType().isLocalDate() || mappingAttribute.getJavaType().isLocalDateTime()) {
				importPackage("jakarta.xml.bind.annotation.adapters");
				importPackage("net.codecadenza.runtime.jaxb");

				if (mappingAttribute.getJavaType().isLocalDateTime())
					b.append("@XmlJavaTypeAdapter(type = LocalDateTime.class, value = LocalDateTimeAdapter.class)\n");
				else
					b.append("@XmlJavaTypeAdapter(type = LocalDate.class, value = LocalDateAdapter.class)\n");
			}
		}
		else
			b.append("@XmlTransient\n");

		return b.toString();
	}

	/**
	 * @param mappingAttribute
	 * @return the JSON-B annotations for a given mapping attribute
	 */
	private String addJSONBAnnotations(ExchangeMappingAttribute mappingAttribute) {
		final var b = new StringBuilder();
		final DataExchangeElement exchangeElement = mappingAttribute.getDataExchangeElement(true);
		final DataExchangeAttribute exchangeAttribute = mappingAttribute.getDataExchangeAttribute();
		boolean isTransient = true;
		String name = null;

		if (exchangeElement != null && !exchangeElement.isDisableExternalMapping()) {
			isTransient = false;

			if (!exchangeElement.getName().equals(mappingAttribute.getName()))
				name = exchangeElement.getName();
		}

		if (exchangeAttribute != null && !exchangeAttribute.isDisableExternalMapping()) {
			isTransient = false;

			if (!exchangeAttribute.getName().equals(mappingAttribute.getName()))
				name = exchangeAttribute.getName();
		}

		if (name != null || isTransient) {
			importPackage("jakarta.json.bind.annotation");

			if (name != null)
				b.append("@JsonbProperty(\"" + name + "\")\n");
			else
				b.append("@JsonbTransient\n");
		}

		return b.toString();
	}

	/**
	 * Get the type name that should be used for the given exchange attribute
	 * @param attr
	 * @return the type name
	 */
	private String getTypeName(ExchangeMappingAttribute attr) {
		final JavaType type = attr.getJavaType();

		// By definition, we handle fields of type char by using a String with exactly one character!
		if (type.isChar()) {
			if (attr.getDomainAttribute() == null || attr.getDomainAttribute().getCollectionType() == CollectionTypeEnumeration.NONE)
				return JavaType.STRING;

			if (attr.getDomainAttribute().getCollectionType() == CollectionTypeEnumeration.LIST)
				return "List<" + JavaType.STRING + ">";
			else
				return "Set<" + JavaType.STRING + ">";
		}

		if (attr.getDomainAttribute() != null)
			return attr.getDomainAttribute().getTypeName();

		return type.getName();
	}

}

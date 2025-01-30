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
package net.codecadenza.eclipse.generator.exchange.add;

import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for XML schema files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLSchemaGenerator {
	/**
	 * Private constructor
	 */
	private XMLSchemaGenerator() {

	}

	/**
	 * @param type
	 * @param validator
	 * @param entryList
	 * @param tabs
	 * @return the generated content
	 */
	private static String addSimpleTypeDefinition(JavaType type, DomainAttributeValidator validator,
			EList<ValueListEntry> entryList, String tabs) {
		final var b = new StringBuilder();
		b.append(tabs + "<xs:simpleType>\n");
		b.append(tabs + "\t<xs:restriction base=");

		// The primitive type char is not explicitly defined in JSR-000222 (Java Architecture for XML Binding)! Thus, we map char
		// fields to String!
		if (type.isString() || type.isChar() || type.isEnum() || type.isUUID())
			b.append("\"xs:string\"");
		else if (type.isInteger())
			b.append("\"xs:int\"");
		else if (type.isLong())
			b.append("\"xs:long\"");
		else if (type.isDouble())
			b.append("\"xs:double\"");
		else if (type.isBigDecimal())
			b.append("\"xs:decimal\"");
		else if (type.isFloat())
			b.append("\"xs:float\"");
		else if (type.isBoolean())
			b.append("\"xs:boolean\"");
		else if (type.isByteArray())
			b.append("\"xs:hexBinary\"");
		else if (type.isDateOrCalendar() || type.isLocalDateTime())
			b.append("\"xs:dateTime\"");
		else if (type.isLocalDate())
			b.append("\"xs:date\"");

		b.append(">\n");

		if (type.isString() && validator != null) {
			final Integer minLength = validator.getMinLength();
			final Integer maxLength = validator.getMaxLength();

			if (minLength != null)
				b.append(tabs + "\t\t<xs:minLength value=\"" + minLength + "\"/>\n");

			if (maxLength != null)
				b.append(tabs + "\t\t<xs:maxLength value=\"" + maxLength + "\"/>\n");

			if (validator.getRegularExpression() != null && !validator.getRegularExpression().isEmpty())
				b.append(tabs + "\t\t<xs:pattern value=\"" + validator.getRegularExpression() + "\"/>\n");
		}

		if (type.isChar()) {
			b.append(tabs + "\t\t<xs:minLength value=\"1\"/>\n");
			b.append(tabs + "\t\t<xs:maxLength value=\"1\"/>\n");
		}
		else if (type.isUUID()) {
			b.append(tabs + "\t\t<xs:pattern value=\"");
			b.append("[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}\"/>\n");
		}

		if (entryList != null)
			entryList.forEach(literal -> b.append(tabs + "\t\t<xs:enumeration value=\"" + literal.getItemText() + "\"/>\n"));

		if (type.isEnum()) {
			final var e = (JavaEnum) type;

			e.getEnumerationValues().forEach(literal -> b.append(tabs + "\t\t<xs:enumeration value=\"" + literal.getName() + "\"/>\n"));
		}

		b.append(tabs + "\t</xs:restriction>\n");
		b.append(tabs + "</xs:simpleType>\n");

		return b.toString();
	}

	/**
	 * @param parentElement
	 * @param prefix
	 * @return the schema type definition
	 */
	private static String addTypeDefinition(DataExchangeElement parentElement, String prefix) {
		final var b = new StringBuilder();
		String wrapperElementName = null;
		final var tab = "\t\t";

		b.append("\n\t<xs:complexType name=\"" + parentElement.getTypeName() + "\">\n");
		b.append(tab + "<xs:sequence>\n");

		for (final DataExchangeElement subElement : parentElement.getSubElements()) {
			if (subElement.isDisableExternalMapping())
				continue;

			// Check if this element is wrapped by another one!
			if (subElement.isContainer() && subElement.getWrapperElementName() != null && !subElement.getWrapperElementName().isEmpty())
				wrapperElementName = subElement.getWrapperElementName();

			if (wrapperElementName != null) {
				b.append(tab + "\t<xs:element maxOccurs=\"1\" minOccurs=\"1\" name=\"" + wrapperElementName + "\">\n");
				b.append(tab + "\t\t<xs:complexType>\n");
				b.append(tab + "\t\t<xs:sequence>\n");
				b.append(tab + "\t\t\t");
			}
			else
				b.append(tab + "\t");

			b.append("<xs:element name=\"" + subElement.getName() + "\" ");

			if (subElement.isContainer()) {
				b.append("type=\"");

				if (!prefix.isEmpty())
					b.append(prefix + ":");

				b.append(subElement.getTypeName() + "\" ");
			}

			if (subElement.getMinOccurrences() != null)
				b.append("minOccurs=\"" + subElement.getMinOccurrences().toString() + "\" ");

			if (subElement.getMaxOccurrences() == null)
				b.append("maxOccurs=\"unbounded\"");
			else
				b.append("maxOccurs=\"" + subElement.getMaxOccurrences().toString() + "\"");

			if (!subElement.isContainer()) {
				b.append(">\n");
				final var tabType = tab + "\t\t";

				JavaType type = subElement.getDataType();

				if (type == null) {
					type = subElement.getMappingAttribute().getMappingType();

					if (type == null)
						type = subElement.getMappingAttribute().getDomainAttribute().getJavaType();
				}

				if (subElement.getMappingAttribute() != null && subElement.getMappingAttribute().getDomainAttribute() != null)
					b.append(
							addSimpleTypeDefinition(type, subElement.getMappingAttribute().getDomainAttribute().getDomainAttributeValidator(),
									subElement.getValueListEntries(), tabType));
				else
					b.append(addSimpleTypeDefinition(type, null, subElement.getValueListEntries(), tabType));

				b.append(tab + "\t</xs:element>\n");
			}
			else
				b.append("/>\n");

			if (wrapperElementName != null) {
				b.append(tab + "\t\t</xs:sequence>\n");
				b.append(tab + "\t\t</xs:complexType>\n");
				b.append(tab + "\t</xs:element>\n");
			}
		}

		b.append("\t\t</xs:sequence>\n");

		for (final DataExchangeAttribute attribute : parentElement.getAttributes()) {
			if (attribute.isDisableExternalMapping())
				continue;

			final var tabType = tab + "\t";

			b.append("\t\t<xs:attribute name=\"" + attribute.getName() + "\"");

			if (!attribute.isOptional())
				b.append(" use=\"required\"");

			b.append(">\n");

			JavaType type = attribute.getDataType();

			if (type == null) {
				type = attribute.getMappingAttribute().getMappingType();

				if (type == null)
					type = attribute.getMappingAttribute().getDomainAttribute().getJavaType();
			}

			if (attribute.getMappingAttribute() != null && attribute.getMappingAttribute().getDomainAttribute() != null)
				b.append(addSimpleTypeDefinition(type, attribute.getMappingAttribute().getDomainAttribute().getDomainAttributeValidator(),
						attribute.getValueListEntries(), tabType));
			else
				b.append(addSimpleTypeDefinition(type, null, attribute.getValueListEntries(), tabType));

			b.append("\t\t</xs:attribute>\n");
		}

		b.append("\t</xs:complexType>\n");

		for (final DataExchangeElement subElement : parentElement.getSubElements())
			if (subElement.isContainer())
				b.append(addTypeDefinition(subElement, prefix));

		return b.toString();
	}

	/**
	 * @param rootElement
	 * @return the generated content
	 */
	public static String createXMLSchema(DataExchangeElement rootElement) {
		final var b = new StringBuilder();
		final DataExchangeMethod method = rootElement.getDataExchangeMethod();
		final Project project = method.getDataExchangeServiceBean().getNamespace().getProject();
		final var prefix = project.getXmlNamespacePrefix() != null ? project.getXmlNamespacePrefix() : "";
		final var charset = method.getCharset() != null && !method.getCharset().isEmpty() ? method.getCharset() : UTF_8;

		b.append("<?xml version=\"1.0\" encoding=\"" + charset + "\" standalone=\"no\"?>\n");
		b.append("<xs:schema ");

		if (!prefix.isEmpty())
			b.append("targetNamespace=\"" + project.getXmlNamespace() + "\" ");

		b.append("xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" ");

		if (!prefix.isEmpty())
			b.append("xmlns:" + prefix + "=\"" + project.getXmlNamespace() + "\" ");

		b.append("attributeFormDefault=\"unqualified\" elementFormDefault=\"qualified\">\n\n");
		b.append("\t<xs:element name=\"" + rootElement.getName() + "\" type=\"");

		if (!prefix.isEmpty())
			b.append(prefix + ":");

		b.append(rootElement.getTypeName() + "\"/>\n");
		b.append(addTypeDefinition(rootElement, prefix));
		b.append("</xs:schema>");

		return b.toString();
	}

}

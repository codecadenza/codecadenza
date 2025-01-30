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
package net.codecadenza.eclipse.generator.exchange.method.imp;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_MAPPING_OBJ_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for CSV data import methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CSVImportMethodGenerator extends AbstractImportMethodGenerator {
	private final CSVMethodGeneratorUtility util;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public CSVImportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);

		this.util = new CSVMethodGeneratorUtility(method);
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

		generator.addImports(util.getImports());

		if (!method.isProcessSingleObject() || method.getExchangeMode() instanceof FileExchangeMode)
			generator.importPackage("java.util");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator#createAdditionalMethods()
	 */
	@Override
	public void createAdditionalMethods() {
		final DataExchangeElement rootElement = method.getRootElement();
		final ExchangeMappingObject mappingObject = rootElement.getMappingObject();
		String methodSignature;

		super.createAdditionalMethods();

		if (method.isProcessSingleObject())
			methodSignature = mappingObject.getName() + " ";
		else
			methodSignature = "List<" + mappingObject.getName() + "> ";

		methodSignature += PARSER_METHOD_PREFIX + mappingObject.getDomainObject().getName() + PARSER_METHOD_SUFFIX;
		methodSignature += "(String content, " + mappingObject.getName() + " type)";

		addMethodToImportSheet(methodSignature, rootElement, !method.isProcessSingleObject());
	}

	/**
	 * @param methodSignature
	 * @param element
	 * @param processMultipleObjects
	 */
	private void addMethodToImportSheet(String methodSignature, DataExchangeElement element, boolean processMultipleObjects) {
		final var b = new StringBuilder();
		final String mappingClassName = element.getMappingObject().getName();
		final String mappingObjectName = element.getMappingObject().getDomainObject().getLowerCaseName();

		b.append("/**\n");
		b.append(" * Parse CSV content\n");
		b.append(" * @param content\n");
		b.append(" * @param type has no functional impact. It is only used to ensure a unique method signature!\n");
		b.append(" * @throws Exception if parsing has failed\n");
		b.append(" * @return ");

		if (processMultipleObjects)
			b.append("a list containing all mapping objects\n");
		else
			b.append("the mapping object to be imported\n");

		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + " throws Exception\n");
		b.append("{\n");

		if (processMultipleObjects)
			b.append("final var " + DATA_IMPORT_LIST + " = new ArrayList<" + mappingClassName + ">();\n");

		b.append("var " + mappingObjectName + " = new " + mappingClassName + "();\n\n");

		// Create the format generator
		b.append("// Create import format\n");
		b.append(util.generateFormatGenerator());
		b.append("// Parse CSV content\n");
		b.append("final CSVParser parser = CSVParser.parse(content, format);\n");
		b.append("final var defaultEmptyValue = \"" + CSVMethodGeneratorUtility.DEFAULT_NULL_VALUE + "\";\n");
		b.append("int rowIndex = 0;\n");
		b.append("int colIndex = -1;\n");
		b.append("String stringValue = null;\n\n");
		b.append("for(final CSVRecord csvRecord : parser.getRecords())\n");
		b.append("{\n");
		b.append("// Skip first row as it contains the header!\n");
		b.append("if(rowIndex == 0)\n");
		b.append("{\n");
		b.append("rowIndex++;\n");
		b.append("continue;\n");
		b.append("}\n\n");

		if (processMultipleObjects) {
			b.append("if(colIndex == -1)\n");
			b.append("{\n");
			b.append(mappingObjectName + " = new " + mappingClassName + "();\n");
			b.append(DATA_IMPORT_LIST + ".add(" + mappingObjectName + ");\n");
			b.append("}\n\n");
		}

		boolean firstAttr = true;

		// Iterate over all attributes
		for (final DataExchangeAttribute attr : element.getAttributes()) {
			ExchangeMappingAttribute mappingAttr = null;
			JavaType type = null;

			if (attr.isDisableExternalMapping())
				continue;

			if (attr.getMappingAttribute() != null) {
				mappingAttr = attr.getMappingAttribute();
				type = mappingAttr.getJavaType();
			}
			else
				type = attr.getDataType();

			if (firstAttr)
				firstAttr = false;
			else
				b.append("\n");

			// We must read all columns. But we cannot do anything more if the column isn't mapped!
			b.append("// Read contents of cell in column \"" + attr.getName() + "\"\n");
			b.append("stringValue = csvRecord.get(++colIndex);\n");

			if (mappingAttr == null)
				continue;

			b.append("\nif(stringValue != null");

			if (!type.isString())
				b.append(" && !stringValue.isEmpty()");

			b.append(" && !stringValue.equals(defaultEmptyValue))\n");

			if (type.isString() || type.isChar())
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(stringValue);\n");
			else if (type.isInteger()) {
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(");

				if (attr.getFormat() == null || attr.getFormat().isEmpty())
					b.append("Integer.parseInt(stringValue));\n");
				else
					b.append("new java.text.DecimalFormat(\"" + attr.getFormat() + "\").parse(stringValue).intValue());\n");
			}
			else if (type.isLong()) {
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(");

				if (attr.getFormat() == null || attr.getFormat().isEmpty())
					b.append("Long.parseLong(stringValue));\n");
				else
					b.append("new java.text.DecimalFormat(\"" + attr.getFormat() + "\").parse(stringValue).longValue());\n");
			}
			else if (type.isDouble()) {
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(");

				if (attr.getFormat() == null || attr.getFormat().isEmpty())
					b.append("Double.parseDouble(stringValue));\n");
				else
					b.append("new java.text.DecimalFormat(\"" + attr.getFormat() + "\").parse(stringValue).doubleValue());\n");
			}
			else if (type.isFloat()) {
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(");

				if (attr.getFormat() == null || attr.getFormat().isEmpty())
					b.append("Double.parseFloat(stringValue));\n");
				else
					b.append("new java.text.DecimalFormat(\"" + attr.getFormat() + "\").parse(stringValue).floatValue());\n");
			}
			else if (type.isDate()) {
				var format = "";

				if (attr.getFormat() != null && !attr.getFormat().isEmpty())
					format = attr.getFormat();

				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(");
				b.append("new java.text.SimpleDateFormat(\"" + format + "\").parse(stringValue));\n");
			}
			else if (type.isCalendar()) {
				// It's up to the developer to provide a proper format!
				var format = "";

				if (attr.getFormat() != null && !attr.getFormat().isEmpty())
					format = attr.getFormat();

				b.append("{\n");
				b.append("final var cal = new GregorianCalendar();\n");
				b.append("cal.setTime(new java.text.SimpleDateFormat(\"" + format + "\").parse(stringValue));\n");
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(cal);\n");
				b.append("}\n");
			}
			else if (type.isLocalDate() || type.isLocalDateTime()) {
				// It's up to the developer to provide a proper format!
				var format = "";

				if (attr.getFormat() != null && !attr.getFormat().isEmpty())
					format = attr.getFormat();

				b.append("{\n");
				b.append("final " + type.getNamespace().toString() + "." + type.getName() + " date = " + type.getNamespace().toString());
				b.append("." + type.getName() + ".from(" + PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ofPattern(");
				b.append("\"" + format + "\").withZone(" + PACK_JAVA_TIME + ".ZoneId.systemDefault()).parse(stringValue));\n");
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(date);\n");
				b.append("}\n");
			}
			else if (type.isBigDecimal()) {
				// It's up to the developer to provide a proper format!
				var format = "";

				if (attr.getFormat() != null && !attr.getFormat().isEmpty())
					format = attr.getFormat();

				b.append("{\n");
				b.append("final var df = new java.text.DecimalFormat(\"" + format + "\");\n");
				b.append("df.setParseBigDecimal(true);\n");
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "((java.math.BigDecimal) df.parse(stringValue));\n");
				b.append("}\n");
			}
			else if (type.isBoolean())
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(Boolean.parseBoolean(stringValue));\n");
			else if (type.isUUID())
				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(java.util.UUID.fromString(stringValue));\n");

			if (type.isEnum()) {
				final var javaEnum = (JavaEnum) type;

				b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(" + javaEnum.getName() + ".valueOf(stringValue));\n");

				if (mappingAttr.getDefaultValue() == null || mappingAttr.getDefaultValue().isEmpty()) {
					var defaultEmptyValue = "null";

					// An enumeration should be initialized if possible!
					if (!javaEnum.getEnumerationValues().isEmpty())
						defaultEmptyValue = javaEnum.getName() + "." + javaEnum.getEnumerationValues().get(0).getName();

					b.append("else\n");
					b.append(mappingObjectName + "." + mappingAttr.getSetterName() + "(" + defaultEmptyValue + ");\n");
				}
			}
		}

		if (processMultipleObjects)
			b.append("\ncolIndex = -1;\n");
		else
			b.append("\nbreak;\n");

		b.append("}\n\n");
		b.append("return ");

		if (processMultipleObjects)
			b.append(DATA_IMPORT_LIST);
		else
			b.append(mappingObjectName);

		b.append(";\n");
		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator#addUnmarshallerInvocation()
	 */
	@Override
	protected String addUnmarshallerInvocation() {
		final var b = new StringBuilder();
		final DataExchangeElement rootElement = method.getRootElement();
		final String rootMappingClassName = rootElement.getMappingObject().getName();
		String mappingObjectName = DEFAULT_MAPPING_OBJ_NAME;
		var contentName = "fileContent";

		if (method.getExchangeMode() instanceof StringExchangeMode) {
			// The first parameter should contain the content!
			final MethodParameter param = method.getMethodParameters().get(0);
			contentName = param.getName();
		}

		if (!method.isProcessSingleObject())
			mappingObjectName = DEFAULT_LIST_ITEM_NAME;

		if (method.isProcessSingleObject())
			b.append("final " + rootMappingClassName + " " + mappingObjectName);
		else
			b.append("final List<" + rootMappingClassName + "> " + DATA_IMPORT_LIST);

		b.append(" = ");

		final String methodName = PARSER_METHOD_PREFIX + rootElement.getMappingObject().getDomainObject().getName()
				+ PARSER_METHOD_SUFFIX;

		b.append(methodName + "(" + contentName + ", new " + rootMappingClassName + "());\n\n");

		return b.toString();
	}

}

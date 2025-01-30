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

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for CSV data export methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CSVExportMethodGenerator extends AbstractExportMethodGenerator {
	private final CSVMethodGeneratorUtility util;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public CSVExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);

		this.util = new CSVMethodGeneratorUtility(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#addImports()
	 */
	@Override
	public void addImports() {
		super.addImports();

		if (generator == null)
			return;

		generator.addImports(util.getImports());
		generator.importPackage("java.io");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#createAdditionalMethods()
	 */
	@Override
	public void createAdditionalMethods() {
		final String sourceListName = DEFAULT_LIST_NAME;
		final DataExchangeElement rootElement = method.getRootElement(true);
		final ExchangeMappingObject mappingObject = rootElement.getMappingObject();
		final String sourceObjName = mappingObject.getDomainObject().getLowerCaseName();
		var methodSignature = "void " + EXPORT_METHOD_PREFIX + mappingObject.getDomainObject().getName() + "(";

		super.createAdditionalMethods();

		if (method.isProcessSingleObject())
			methodSignature += mappingObject.getName() + " " + sourceObjName;
		else
			methodSignature += "List<" + mappingObject.getName() + "> " + sourceListName;

		methodSignature += ", CSVPrinter printer)";

		createMethodToExportSheet(methodSignature, sourceObjName, sourceListName, rootElement, !method.isProcessSingleObject());
	}

	/**
	 * @param methodSignature
	 * @param sourceObjectName
	 * @param sourceListName
	 * @param element
	 * @param processMultipleObjects
	 */
	private void createMethodToExportSheet(String methodSignature, String sourceObjectName, String sourceListName,
			DataExchangeElement element, boolean processMultipleObjects) {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Export data in CSV format\n");

		if (processMultipleObjects)
			b.append(" * @param " + sourceListName + "\n");
		else
			b.append(" * @param " + sourceObjectName + "\n");

		b.append(" * @param printer\n");
		b.append(" * @throws Exception if printing has failed\n");
		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + " throws Exception\n");
		b.append("{\n");

		// Create the header row
		b.append("// Create header row\n");

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping())
				continue;

			b.append("printer.print(\"" + attr.getName() + "\");\n");
		}

		b.append("printer.println();\n\n");

		if (processMultipleObjects) {
			b.append("for(final " + element.getMappingObject().getName() + " " + sourceObjectName + " : " + sourceListName + ")\n");
			b.append("{\n");
		}

		boolean firstAttr = true;

		// Iterate over all attributes
		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping())
				continue;

			ExchangeMappingAttribute mappingAttr = null;
			JavaType type = null;

			if (attr.getMappingAttribute() != null) {
				mappingAttr = attr.getMappingAttribute();
				type = mappingAttr.getJavaType();
			}
			else
				type = attr.getDataType();

			boolean format = false;

			if (firstAttr)
				firstAttr = false;
			else
				b.append("\n");

			b.append("// Write field value to column \"" + attr.getName() + "\"\n");

			if (mappingAttr != null && attr.getFormat() != null && !attr.getFormat().isEmpty()) {
				var formatString = "";

				if (type.isDateOrCalendar()) {
					formatString = "new java.text.SimpleDateFormat(\"" + attr.getFormat() + "\").format(";
					format = true;
				}
				else if (type.isLocalDate() || type.isLocalDateTime()) {
					formatString = PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ofPattern(";
					formatString += "\"" + attr.getFormat() + "\").withZone(" + PACK_JAVA_TIME + ".ZoneId.systemDefault()).format(";

					format = true;
				}
				else if (type.isNumber()) {
					formatString = "new java.text.DecimalFormat(\"" + attr.getFormat() + "\").format(";
					format = true;
				}

				if (format && !type.isPrimitive())
					b.append("if(" + sourceObjectName + "." + mappingAttr.getGetterName() + " != null)\n");

				b.append("printer.print(");
				b.append(formatString);
			}
			else
				b.append("printer.print(");

			if (mappingAttr != null) {
				b.append(sourceObjectName + "." + mappingAttr.getGetterName());

				if (type.isEnum())
					b.append(".name()");

				if (type.isCalendar() && format)
					b.append(".getTime()");
			}
			else if (type.isPrimitive()) {
				if (type.isType(JavaType.INT, JavaType.LONG))
					b.append("0");
				else if (type.isType(JavaType.DOUBLE, JavaType.FLOAT))
					b.append("0.0");
				else if (type.isType(JavaType.BOOL))
					b.append("false");
				else if (type.isChar())
					b.append("'a'");
			}
			else
				b.append("null");

			if (format)
				b.append(")");

			b.append(");\n");

			if (format && !type.isPrimitive()) {
				b.append("else\n");
				b.append("printer.print(null);\n");
			}
		}

		if (processMultipleObjects) {
			b.append("\nprinter.println();\n");
			b.append("}\n");
		}

		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#addMarshallerInvocation(java.lang.String)
	 */
	@Override
	protected String addMarshallerInvocation(String marshalledObject) {
		final var b = new StringBuilder();
		final DataExchangeElement rootElement = method.getRootElement(true);

		// Create the format generator
		b.append("\n// Create export format\n");
		b.append(util.generateFormatGenerator());

		if (method.getExchangeMode() instanceof FileExchangeMode)
			b.append("final var content = new StringBuilder();\n\n");

		// Create the printer
		b.append("// Create output printer\n");
		b.append("final var printer = new CSVPrinter(");

		if (method.getExchangeMode() instanceof FileExchangeMode)
			b.append("content");
		else
			b.append("outputWriter");

		b.append(", format);\n\n");

		// Invoke the method that prints out data!
		final String methodName = EXPORT_METHOD_PREFIX + rootElement.getMappingObject().getDomainObject().getName();

		b.append("// Print data\n");
		b.append(methodName + "(" + marshalledObject + ", printer);\n\n");

		if (method.getExchangeMode() instanceof FileExchangeMode) {
			b.append("try(final var fileOutputStream = new FileOutputStream(outputFile))\n");
			b.append("{\n");
			b.append("final byte[] contentInBytes = content.toString().getBytes();\n");
			b.append("fileOutputStream.write(contentInBytes);\n");
			b.append("}\n");

			if (method.returnsPath())
				b.append("\nreturn outputFile.getAbsolutePath();\n");
		}
		else
			b.append("return outputWriter.toString();\n");

		return b.toString();
	}

}

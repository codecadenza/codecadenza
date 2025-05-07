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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;

/**
 * <p>
 * Generator for XML data export methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLExportMethodGenerator extends AbstractExportMethodGenerator {
	private final XMLSchemaValidationGenerator xmlSchemaValidationGenerator;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public XMLExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);

		this.xmlSchemaValidationGenerator = new XMLSchemaValidationGenerator(generator, method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#addImports()
	 */
	@Override
	public void addImports() {
		super.addImports();

		if (directExchangeMode || generator == null)
			return;

		generator.importPackage("jakarta.xml.bind");
		generator.importPackage("java.io");

		xmlSchemaValidationGenerator.addImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#
	 * addMarshallerInvocation(java.lang.String)
	 */
	@Override
	protected String addMarshallerInvocation(String marshalledObject) {
		final var b = new StringBuilder();
		final String rootMappingClassName = method.getRootElement(true).getMappingObject().getName();

		b.append("final Marshaller marshaller = JAXBContext.newInstance(" + rootMappingClassName + ".class).createMarshaller();\n");
		b.append(xmlSchemaValidationGenerator.createSchemaInitFragment("marshaller"));

		if (method.isFormatOutput())
			b.append("marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);\n");

		if (method.getCharset() != null && !method.getCharset().isEmpty())
			b.append("marshaller.setProperty(\"jaxb.encoding\", \"" + method.getCharset() + "\");\n");

		if (stringExchangeMode) {
			b.append("marshaller.marshal(" + marshalledObject + ", outputWriter);\n\n");
			b.append("return outputWriter.toString();\n");
		}

		if (fileExchangeMode != null) {
			b.append("marshaller.marshal(" + marshalledObject + ", outputFile);\n");

			if (method.returnsPath())
				b.append("\nreturn outputFile.getAbsolutePath();\n");
		}

		return b.toString();
	}

}

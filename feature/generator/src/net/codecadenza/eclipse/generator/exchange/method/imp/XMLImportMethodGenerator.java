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
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_ROOT_MAPPING_OBJ_NAME;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for XML data import methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLImportMethodGenerator extends AbstractImportMethodGenerator {
	private final XMLSchemaValidationGenerator xmlSchemaValidationGenerator;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public XMLImportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);

		this.xmlSchemaValidationGenerator = new XMLSchemaValidationGenerator(generator, method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#addImports()
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
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator#addUnmarshallerInvocation()
	 */
	@Override
	protected String addUnmarshallerInvocation() {
		final var b = new StringBuilder();
		final String rootMappingClassName = method.getRootElement().getMappingObject().getName();

		b.append("final Unmarshaller unmarshaller = JAXBContext.newInstance(");
		b.append(rootMappingClassName + ".class).createUnmarshaller();\n");
		b.append(xmlSchemaValidationGenerator.createSchemaInitFragment("unmarshaller"));
		b.append("\n");
		b.append("final var ");

		if (method.isProcessSingleObject())
			b.append(DEFAULT_MAPPING_OBJ_NAME);
		else
			b.append(DEFAULT_ROOT_MAPPING_OBJ_NAME);

		b.append(" = (" + rootMappingClassName + ") unmarshaller.unmarshal(");

		if (fileExchangeMode == null) {
			// We expect exactly one parameter!
			final MethodParameter param = method.getMethodParameters().get(0);

			b.append("new StringReader(" + param.getName() + ")");
		}
		else
			b.append("inputReader");

		b.append(");\n\n");

		return b.toString();
	}

}

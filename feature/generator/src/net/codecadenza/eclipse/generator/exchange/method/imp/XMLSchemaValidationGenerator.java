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

import static net.codecadenza.eclipse.shared.Constants.SCHEMA_FOLDER;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the validation of an XML document by using a respective schema file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLSchemaValidationGenerator {
	private final AbstractJavaSourceGenerator generator;
	private final DataExchangeMethod method;
	private final Project project;
	private final String schemaFileName;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public XMLSchemaValidationGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		this.generator = generator;
		this.method = method;
		this.project = method.getDataExchangeServiceBean().getNamespace().getProject();
		this.schemaFileName = method.getRootElement(true).getDataExchangeMethod().getSchemaFileName();
	}

	/**
	 * Add all required imports if the XML schema validation is required
	 */
	public void addImports() {
		if (generator == null || !isSchemaValidationRequired())
			return;

		generator.importPackage("javax.xml.validation");

		if (project.isSpringBootApplication()) {
			generator.importPackage("javax.xml.transform.stream");
			generator.importPackage("org.springframework.core.io");
		}
	}

	/**
	 * @param schemaConsumerName
	 * @return the generated content
	 */
	public String createSchemaInitFragment(String schemaConsumerName) {
		final var b = new StringBuilder();

		if (!isSchemaValidationRequired())
			return b.toString();

		b.append("final SchemaFactory sf = SchemaFactory.newInstance(javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI);\n");

		if (project.isSpringBootApplication()) {
			b.append("final Schema schema = sf.newSchema(new StreamSource(new ClassPathResource(\"");
			b.append(SCHEMA_FOLDER + "/" + schemaFileName + "\").getInputStream()));\n");
		}
		else {
			b.append("final Schema schema = sf.newSchema(getClass().getClassLoader()");
			b.append(".getResource(\"" + SCHEMA_FOLDER + "/" + schemaFileName + "\"));\n");
		}

		b.append("\n");
		b.append(schemaConsumerName + ".setSchema(schema);\n");

		return b.toString();
	}

	/**
	 * @return true if the XML schema validation is required
	 */
	private boolean isSchemaValidationRequired() {
		return method.isPerformValidation() && schemaFileName != null && !schemaFileName.isEmpty();
	}

}

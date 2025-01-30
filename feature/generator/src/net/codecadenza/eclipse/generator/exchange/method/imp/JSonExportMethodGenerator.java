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
 * Generator for JSon data export methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSonExportMethodGenerator extends AbstractExportMethodGenerator {
	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public JSonExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);
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

		generator.importPackage("jakarta.json.bind");
		generator.importPackage("java.io");

		if (project.isSpringBootApplication())
			generator.importPackage("jakarta.json.bind.annotation");

		if (fileExchangeMode != null)
			generator.importPackage("java.nio.file");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#addMarshallerInvocation(java.lang.String)
	 */
	@Override
	protected String addMarshallerInvocation(String marshalledObject) {
		final var b = new StringBuilder();

		if (project.isSpringBootApplication() || method.isFormatOutput()) {
			b.append("final var config = new JsonbConfig()");

			if (method.isFormatOutput())
				b.append(".withFormatting(true)");

			b.append(";\n");

			// In case of a Spring Boot application, it is necessary to convert date values to milliseconds! Otherwise, subsequent
			// import operations may fail because these values cannot be parsed!
			if (project.isSpringBootApplication())
				b.append("config.setProperty(JsonbConfig.DATE_FORMAT, JsonbDateFormat.TIME_IN_MILLIS);\n");

			b.append("\n");
			b.append("final Jsonb jsonb = JsonbBuilder.create(config);\n");
		}
		else
			b.append("final Jsonb jsonb = JsonbBuilder.create();\n");

		if (stringExchangeMode) {
			b.append("jsonb.toJson(" + marshalledObject + ", outputWriter);\n");
			b.append("jsonb.close();\n\n");
			b.append("return outputWriter.toString();\n");
		}

		if (fileExchangeMode != null) {
			b.append("jsonb.toJson(" + marshalledObject);
			b.append(", Files.newOutputStream(outputFile.toPath(), StandardOpenOption.CREATE));\n");
			b.append("jsonb.close();\n");

			if (method.returnsPath())
				b.append("\nreturn outputFile.getAbsolutePath();\n");
		}

		return b.toString();
	}

}

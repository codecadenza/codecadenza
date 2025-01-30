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
package net.codecadenza.eclipse.generator.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * Generator for adding logging statements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingGenerator {
	public static final String LOGGER_FIELD_NAME = "logger";

	private final boolean protectManualChanges;

	/**
	 * Constructor
	 * @param protectManualChanges
	 */
	public LoggingGenerator(boolean protectManualChanges) {
		this.protectManualChanges = protectManualChanges;
	}

	/**
	 * Add a field for the logger
	 * @param b the {@link StringBuilder} where the generated content should be added to
	 */
	public void addField(StringBuilder b) {
		final var fieldGenerator = new JavaFieldGenerator("Logger", LOGGER_FIELD_NAME, JavaFieldGenerator.VISIBILITY_PRIVATE, b,
				protectManualChanges);
		fieldGenerator.asConstant("LoggerFactory.getLogger(MethodHandles.lookup().lookupClass())");
		fieldGenerator.create();
	}

	/**
	 * @return a set containing all necessary imports for logging
	 */
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		imports.add("import org.slf4j.*;");
		imports.add("import java.lang.invoke.*;");

		return imports;
	}

	/**
	 * Add a debug log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public static void addDebugLog(StringBuilder b, String message, String... parameters) {
		b.append(LOGGER_FIELD_NAME + ".debug(\"" + message + "\"");
		b.append(createParameterList(null, parameters));
		b.append(");\n");
	}

	/**
	 * Add an info log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public static void addInfoLog(StringBuilder b, String message, String... parameters) {
		b.append(LOGGER_FIELD_NAME + ".info(\"" + message + "\"");
		b.append(createParameterList(null, parameters));
		b.append(");\n");
	}

	/**
	 * Add a warning log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public static void addWarningLog(StringBuilder b, String message, String exception, String... parameters) {
		b.append(LOGGER_FIELD_NAME + ".warn(\"" + message + "\"");
		b.append(createParameterList(exception, parameters));
		b.append(");\n");
	}

	/**
	 * Add an error log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public static void addErrorLog(StringBuilder b, String message, String exception, String... parameters) {
		b.append(LOGGER_FIELD_NAME + ".error(\"" + message + "\"");
		b.append(createParameterList(exception, parameters));
		b.append(");\n");
	}

	/**
	 * Add an error log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param constantName the name of the constant that holds the actual error message
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public static void addErrorLogFromConstant(StringBuilder b, String constantName, String exception, String... parameters) {
		b.append(LOGGER_FIELD_NAME + ".error(" + constantName);
		b.append(createParameterList(exception, parameters));
		b.append(");\n");
	}

	/**
	 * Create the log message parameter list
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 * @return the generated content
	 */
	private static String createParameterList(String exception, String... parameters) {
		final var b = new StringBuilder();
		final List<String> parameterList = new ArrayList<>(Arrays.asList(parameters));

		// Add the exception to the end of the list in order to let SLF4J properly recognize it!
		if (exception != null && !exception.isEmpty())
			parameterList.add(exception);

		parameterList.forEach(param -> b.append(", " + param));

		return b.toString();
	}

}

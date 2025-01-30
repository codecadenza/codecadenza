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
package net.codecadenza.eclipse.tools.angular;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import net.codecadenza.eclipse.tools.util.os.OperatingSystem;
import org.eclipse.core.resources.IProject;

/**
 * <p>
 * Utility for creating an Angular project within an existing Eclipse project
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularProjectBuildService {
	private static final String SYSTEM_ENVIRONMENT_PATH = "PATH";
	private static final String ANGULAR_VERSION = "17.0";
	private static final String CHART_JS_VERSION = "4.4";
	private static final String CRYPTO_ES_VERSION = "2.1";
	private static final String FULLCALENDAR_VERSION = "6.1";
	private static final String PRIMEICONS_VERSION = "6.0";
	private static final String PRIMENG_VERSION = "17.3";
	private static final String PRIMEFLEX_VERSION = "3.3";
	private static final String QUILL_VERSION = "1.3";
	private static final String UUID_VERSION = "9.0";
	private static final String DEPENDENCY_CDK = "@angular/cdk";
	private static final String DEPENDENCY_CHART_JS = "chart.js";
	private static final String DEPENDENCY_CRYPTO_ES = "crypto-es";
	private static final String DEPENDENCY_FULL_CALENDAR = "@fullcalendar/core";
	private static final String DEPENDENCY_LOCALIZE = "@angular/localize";
	private static final String DEPENDENCY_LINT = "@angular-eslint/schematics";
	private static final String DEPENDENCY_PRIMENG = "primeng";
	private static final String DEPENDENCY_PRIMEICONS = "primeicons";
	private static final String DEPENDENCY_PRIMEFLEX = "primeflex";
	private static final String DEPENDENCY_QUILL = "quill";
	private static final String DEPENDENCY_UUID = "@types/uuid";
	private static final String ENVIRONMENTS = "environments";

	private final String projectName;
	private final File projectDirectory;
	private final File parentDirectory;

	/**
	 * Constructor
	 * @param targetProject
	 */
	public AngularProjectBuildService(IProject targetProject) {
		this.projectName = targetProject.getName();
		this.projectDirectory = targetProject.getLocation().toFile();
		this.parentDirectory = targetProject.getLocation().toFile().getParentFile();
	}

	/**
	 * Install the Angular CLI and build the project by using NPM
	 * @throws Exception if the project could not be built
	 */
	public void build() throws Exception {
		// Install the Angular CLI
		installAngular(ANGULAR_VERSION);

		// Create the Angular project
		createAngularProject();

		// Add all dependencies
		addDependency(DEPENDENCY_CDK, ANGULAR_VERSION);
		addDependency(DEPENDENCY_CHART_JS, CHART_JS_VERSION);
		addDependency(DEPENDENCY_CRYPTO_ES, CRYPTO_ES_VERSION);
		addDependency(DEPENDENCY_FULL_CALENDAR, FULLCALENDAR_VERSION);
		addDependency(DEPENDENCY_QUILL, QUILL_VERSION);
		addDependency(DEPENDENCY_PRIMEICONS, PRIMEICONS_VERSION);
		addDependency(DEPENDENCY_PRIMENG, PRIMENG_VERSION);
		addDependency(DEPENDENCY_PRIMEFLEX, PRIMEFLEX_VERSION);
		addDependency(DEPENDENCY_UUID, UUID_VERSION);
	}

	/**
	 * Install the Angular CLI
	 * @param versionNumber
	 * @throws Exception if an internal error has occurred
	 */
	private void installAngular(String versionNumber) throws Exception {
		final List<String> command = createCommand("npm", "install", "-g", "@angular/cli@" + versionNumber);

		runProgram(command, projectDirectory);
	}

	/**
	 * Create a new Angular project
	 * @throws Exception if an internal error has occurred
	 */
	private void createAngularProject() throws Exception {
		final List<String> createCommand = createCommand("ng", "new", projectName, "--skip-git", "--interactive", "false",
				"--no-standalone");
		final List<String> addEnvironmentsCommand = createCommand("ng", "g", ENVIRONMENTS);
		final List<String> addLocalizeCommand = createCommand("ng", "add", DEPENDENCY_LOCALIZE, "--skip-confirmation");
		final List<String> addLintCommand = createCommand("ng", "add", DEPENDENCY_LINT, "--skip-confirmation");

		runProgram(createCommand, parentDirectory);
		runProgram(addEnvironmentsCommand, projectDirectory);
		runProgram(addLocalizeCommand, projectDirectory);
		runProgram(addLintCommand, projectDirectory);
	}

	/**
	 * Add a runtime dependency to the given project
	 * @param name
	 * @param version
	 * @throws Exception if an internal error has occurred
	 */
	private void addDependency(String name, String version) throws Exception {
		final String dependency = name + (version != null ? "@" + version : "");
		final List<String> command = createCommand("npm", "install", "--save", dependency);

		runProgram(command, projectDirectory);
	}

	/**
	 * @param arguments
	 * @return the operating system dependent list of commands for running a program and its arguments
	 */
	private List<String> createCommand(String... arguments) {
		final var command = new ArrayList<String>();

		if (OperatingSystem.isWindows()) {
			command.add("cmd");
			command.add("/c");
		}
		else {
			command.add("sh");
			command.add("-c");
		}

		if (OperatingSystem.isWindows()) {
			// On Windows all arguments are just added to the command list
			command.addAll(Stream.of(arguments).toList());
		}
		else {
			// On Linux and macOS a shell command has only one argument!
			command.add(Stream.of(arguments).reduce((a, b) -> a + " " + b).orElse(""));
		}

		return command;
	}

	/**
	 * Run an operating system program and wait until the operation has been finished
	 * @param command a list containing the program and its arguments
	 * @param workingDirectory
	 * @throws Exception if an internal error has occurred
	 */
	private void runProgram(List<String> command, File workingDirectory) throws Exception {
		final var pb = new ProcessBuilder(command);
		pb.directory(workingDirectory);
		pb.redirectErrorStream(true);
		pb.redirectOutput(ProcessBuilder.Redirect.INHERIT);
		pb.redirectErrorStream();

		if (OperatingSystem.isMacOS())
			pb.environment().put(SYSTEM_ENVIRONMENT_PATH, "/usr/local/bin:/bin");

		pb.start().waitFor();
	}

}

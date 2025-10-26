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
package net.codecadenza.eclipse.service.launch.config;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_APPLICATION_NAME;
import static net.codecadenza.eclipse.shared.Constants.JAVAFX_LAUNCHER_NAME;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.core.runtime.CoreException;

/**
 * <p>
 * Factory for generating launch settings for generated applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LaunchSettingsGeneratorFactory {
	private static final String COMMAND_NG_SERVE = "ng serve";
	private static final String COMMAND_NG_BUILD = "ng build";
	private static final String DEFAULT_ANGULAR_URL = "http://localhost:4200";
	private static final String DEFAULT_WEB_APP_URL = "http://localhost:8080/";

	/**
	 * Prevent instantiation
	 */
	private LaunchSettingsGeneratorFactory() {

	}

	/**
	 * Create all necessary launch settings for running the applications of the given project
	 * @param project the project that should be started
	 * @return a list with launch settings
	 * @throws CoreException if the launch settings could not be created
	 */
	public static List<LaunchSettings> createRunSettings(Project project) throws CoreException {
		final var launchSettings = new ArrayList<LaunchSettings>();
		final String url;

		if (project.hasJSFOrVaadinClient())
			url = DEFAULT_WEB_APP_URL + project.getCode();
		else
			url = null;

		if (project.isSpringBootApplication()) {
			final String projectName;

			if (project.hasJSFOrVaadinClient())
				projectName = project.getTargetProjectName(BuildArtifactType.GUI);
			else
				projectName = project.getTargetProjectName(BuildArtifactType.SERVER);

			final String mainClass = project.getRootNamespace().toString() + "." + DEFAULT_APPLICATION_NAME;

			launchSettings.add(new JavaLaunchSettingsGenerator(projectName, mainClass, true, url).createLaunchSettings());
		}
		else if (project.isJakartaEEApplication()) {
			final String projectName = project.getTargetProjectName(BuildArtifactType.MASTER);

			launchSettings.add(new MavenLaunchSettingsGenerator(projectName, true, url).createLaunchSettings());
		}

		if (project.hasClient()) {
			final String projectName = project.getTargetProjectName(BuildArtifactType.GUI);

			if (project.hasAngularClient()) {
				final var angularLaunchSettings = new ExternalCommandLaunchSettingsGenerator(projectName, true, DEFAULT_ANGULAR_URL,
						COMMAND_NG_SERVE).createLaunchSettings();

				launchSettings.add(angularLaunchSettings);
			}
			else if (project.hasJavaFXClient()) {
				final String mainClass = project.getClientNamespace().toString() + "." + JAVAFX_LAUNCHER_NAME;

				launchSettings.add(new JavaLaunchSettingsGenerator(projectName, mainClass, true, null).createLaunchSettings());
			}
			else if (project.hasSwingClient()) {
				final String mainClass = project.getClientNamespace().toString() + "." + DEFAULT_APPLICATION_NAME;

				launchSettings.add(new JavaLaunchSettingsGenerator(projectName, mainClass, true, null).createLaunchSettings());
			}
			else if (project.hasRCPClient())
				launchSettings.add(new EclipseRCPLaunchSettingsGenerator(projectName, true).createLaunchSettings());
			else if (project.hasRAPClient())
				launchSettings.add(new EclipseRAPLaunchSettingsGenerator(projectName, true).createLaunchSettings());
		}

		return launchSettings;
	}

	/**
	 * Create the launch settings for a JUnit test
	 * @param testToRun the test that should be launched
	 * @return the launch settings
	 * @throws CoreException if the launch settings could not be created
	 */
	public static LaunchSettings createTestSettings(JavaFile testToRun) throws CoreException {
		return new JUnitLaunchSettingsGenerator(true, testToRun).createLaunchSettings();
	}

	/**
	 * Create the launch settings for building a project
	 * @param project the project that should be built
	 * @return a list with launch settings
	 * @throws IllegalStateException when trying to build a standalone Eclipse application
	 * @throws CoreException if the launch settings could not be created
	 */
	public static List<LaunchSettings> createBuildSettings(Project project) throws CoreException {
		if (project.hasEclipseClient() && project.isJavaSEApplication())
			throw new IllegalStateException("Building of a standalone Eclipse application is not supported!");

		final String rootProjectName = project.getTargetProjectName(BuildArtifactType.MASTER);

		final var launchSettings = new ArrayList<LaunchSettings>();
		launchSettings.add(new MavenLaunchSettingsGenerator(rootProjectName, false, null).createLaunchSettings());

		if (project.hasAngularClient()) {
			final String clientProjectName = project.getTargetProjectName(BuildArtifactType.GUI);
			final LaunchSettings angularLaunchSettings = new ExternalCommandLaunchSettingsGenerator(clientProjectName, false, null,
					COMMAND_NG_BUILD).createLaunchSettings();

			launchSettings.add(angularLaunchSettings);
		}

		return launchSettings;
	}

}

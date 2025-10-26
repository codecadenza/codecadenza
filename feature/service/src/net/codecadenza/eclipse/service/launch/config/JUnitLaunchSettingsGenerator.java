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

import static net.codecadenza.eclipse.shared.Constants.JAVA_RESOURCE_SUFFIX;
import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER;
import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME;
import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME;
import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_SOURCE_PATH_PROVIDER;

import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFile;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/**
 * <p>
 * Create the launch settings for running a JUnit tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JUnitLaunchSettingsGenerator extends AbstractLaunchSettingsGenerator {
	private static final String ATTR_MAPPED_RESOURCE_PATH = "org.eclipse.debug.core.MAPPED_RESOURCE_PATHS";
	private static final String ATTR_MAPPED_RESOURCE_TYPES = "org.eclipse.debug.core.MAPPED_RESOURCE_TYPES";
	private static final String ATTR_TEST_KIND = "org.eclipse.jdt.junit.TEST_KIND";
	private static final String CONFIG_TYPE = "org.eclipse.jdt.junit.launchconfig";
	private static final String TEST_KIND = "org.eclipse.jdt.junit.loader.junit5";
	private static final String M2E_CLASSPATH_PROVIDER = "org.eclipse.m2e.launchconfig.classpathProvider";
	private static final String M2E_SOURCE_PATH_PROVIDER = "org.eclipse.m2e.launchconfig.sourcepathProvider";

	private final JavaFile testToRun;

	/**
	 * Constructor
	 * @param saveConfig flag that controls if a new launch configuration should be saved
	 * @param testToRun the representation of the Java source file of the JUnit test that should be executed
	 */
	public JUnitLaunchSettingsGenerator(boolean saveConfig, JavaFile testToRun) {
		super(testToRun.getProjectName(), testToRun.getPackageName() + "." + testToRun.getName().replace(JAVA_RESOURCE_SUFFIX, ""),
				saveConfig, null);

		this.testToRun = testToRun;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.launch.config.AbstractLaunchSettingsGenerator#getLaunchConfigurationType()
	 */
	@Override
	public String getLaunchConfigurationType() {
		return CONFIG_TYPE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.launch.config.AbstractLaunchSettingsGenerator#
	 * initLaunchConfiguration(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
	 */
	@Override
	public void initLaunchConfiguration(ILaunchConfigurationWorkingCopy launchConfig) {
		final String testFolder = testToRun.getPath();
		final String pathToTestFile = testToRun.getPackageName().replace('.', '/');
		final String resourcePath = "/" + projectName + "/" + testFolder + "/" + pathToTestFile + "/" + testToRun.getName();

		launchConfig.setAttribute(ATTR_CLASSPATH_PROVIDER, M2E_CLASSPATH_PROVIDER);
		launchConfig.setAttribute(ATTR_MAIN_TYPE_NAME, launchClassName);
		launchConfig.setAttribute(ATTR_MAPPED_RESOURCE_PATH, List.of(resourcePath));
		launchConfig.setAttribute(ATTR_MAPPED_RESOURCE_TYPES, List.of("1"));
		launchConfig.setAttribute(ATTR_PROJECT_NAME, projectName);
		launchConfig.setAttribute(ATTR_TEST_KIND, TEST_KIND);
		launchConfig.setAttribute(ATTR_SOURCE_PATH_PROVIDER, M2E_SOURCE_PATH_PROVIDER);
	}

}

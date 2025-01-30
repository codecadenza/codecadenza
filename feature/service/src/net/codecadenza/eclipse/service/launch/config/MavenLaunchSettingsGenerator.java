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

import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_WORKING_DIRECTORY;

import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/**
 * <p>
 * Create the launch settings for building a Maven application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MavenLaunchSettingsGenerator extends AbstractLaunchSettingsGenerator {
	private static final String ATTR_GOALS = "M2_GOALS";
	private static final String ATTR_SKIP_TESTS = "M2_SKIP_TESTS";
	private static final String CONFIG_TYPE = "org.eclipse.m2e.Maven2LaunchConfigurationType";
	private static final String GOALS = "clean install";

	/**
	 * Constructor
	 * @param name the name of the launch configuration
	 * @param saveConfig flag that controls if a new launch configuration should be saved
	 * @param webApplicationURL the optional URL of the respective web application
	 */
	public MavenLaunchSettingsGenerator(String name, boolean saveConfig, String webApplicationURL) {
		super(name, saveConfig, webApplicationURL);
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
		launchConfig.setAttribute(ATTR_GOALS, GOALS);
		launchConfig.setAttribute(ATTR_SKIP_TESTS, true);
		launchConfig.setAttribute(ATTR_WORKING_DIRECTORY, getWorkingDirectory());
	}

}

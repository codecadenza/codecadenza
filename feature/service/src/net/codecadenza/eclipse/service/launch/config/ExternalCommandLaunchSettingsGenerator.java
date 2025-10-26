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

import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME;

import net.codecadenza.eclipse.tools.util.os.OperatingSystem;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/**
 * <p>
 * Create the launch settings for executing an external command
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExternalCommandLaunchSettingsGenerator extends AbstractLaunchSettingsGenerator {
	private static final String ATTR_ARGUMENTS = "org.eclipse.ui.externaltools.ATTR_TOOL_ARGUMENTS";
	private static final String ATTR_LOCATION = "org.eclipse.ui.externaltools.ATTR_LOCATION";
	private static final String ATTR_WORKING_DIR = "org.eclipse.ui.externaltools.ATTR_WORKING_DIRECTORY";
	private static final String CONFIG_TYPE = "org.eclipse.ui.externaltools.ProgramLaunchConfigurationType";
	private static final String MAC_SHELL = "/bin/zsh";
	private static final String LINUX_SHELL = "/bin/bash";
	private static final String WINDOWS_CMD = "C:\\Windows\\System32\\cmd.exe";

	private final String command;

	/**
	 * Constructor
	 * @param projectName the name of the launch configuration
	 * @param saveConfig flag that controls if a new launch configuration should be saved
	 * @param webApplicationURL the optional URL of the respective web application
	 * @param command the command to be executed
	 */
	public ExternalCommandLaunchSettingsGenerator(String projectName, boolean saveConfig, String webApplicationURL,
			String command) {
		super(projectName, null, saveConfig, webApplicationURL);

		this.command = command;
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
		launchConfig.setAttribute(ATTR_PROJECT_NAME, projectName);
		launchConfig.setAttribute(ATTR_WORKING_DIR, getWorkingDirectory());

		if (OperatingSystem.isWindows()) {
			launchConfig.setAttribute(ATTR_LOCATION, WINDOWS_CMD);
			launchConfig.setAttribute(ATTR_ARGUMENTS, "/c " + command);
		}
		else {
			launchConfig.setAttribute(ATTR_ARGUMENTS, "-c \"" + command + "\"");

			if (OperatingSystem.isMacOS())
				launchConfig.setAttribute(ATTR_LOCATION, MAC_SHELL);
			else
				launchConfig.setAttribute(ATTR_LOCATION, LINUX_SHELL);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.launch.config.AbstractLaunchSettingsGenerator#isDebugSupported()
	 */
	@Override
	protected boolean isDebugSupported() {
		// An external command must not be started in debug mode!
		return false;
	}

}

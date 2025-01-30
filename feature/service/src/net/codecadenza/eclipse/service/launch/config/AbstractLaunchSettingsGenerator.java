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

import java.time.Duration;
import java.util.Collections;
import java.util.Map;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;

/**
 * <p>
 * Base class for all launch setting generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractLaunchSettingsGenerator {
	public static final String ATTR_ENV_VARIABLES = "org.eclipse.debug.core.environmentVariables";
	private static final Duration DEFAULT_TIME_OUT = Duration.ofMinutes(1);
	private static final Duration INITIAL_TIME_OUT = Duration.ofMinutes(3);
	public static final String ENV_VARIABLE_URL = "WEB_APPLICATION_URL";

	private final ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
	protected final String name;
	protected final boolean saveConfig;
	protected final String webApplicationURL;

	/**
	 * Constructor
	 * @param name the name of the launch configuration
	 * @param saveConfig flag that controls if a new launch configuration should be saved
	 * @param webApplicationURL the optional URL of the respective web application
	 */
	protected AbstractLaunchSettingsGenerator(String name, boolean saveConfig, String webApplicationURL) {
		this.name = name;
		this.saveConfig = saveConfig;
		this.webApplicationURL = webApplicationURL;
	}

	/**
	 * Initialize a new launch configuration
	 * @param launchConfig the launch configuration to be initialized
	 */
	protected abstract void initLaunchConfiguration(ILaunchConfigurationWorkingCopy launchConfig);

	/**
	 * Get the launch configuration type
	 * @return the launch configuration type
	 */
	protected abstract String getLaunchConfigurationType();

	/**
	 * Get or create new launch settings
	 * @return the launch settings
	 * @throws CoreException if the launch configuration could not be created
	 */
	public LaunchSettings createLaunchSettings() throws CoreException {
		final ILaunchConfigurationType launchConfigType = launchManager.getLaunchConfigurationType(getLaunchConfigurationType());
		final ILaunchConfiguration existingLaunchConfiguration = searchExistingLaunchConfiguration();

		if (existingLaunchConfiguration != null) {
			String urlToUse = getWebApplicationURL(existingLaunchConfiguration);

			if (urlToUse == null)
				urlToUse = webApplicationURL;

			return new LaunchSettings(existingLaunchConfiguration, urlToUse, DEFAULT_TIME_OUT, isDebugSupported());
		}

		final ILaunchConfigurationWorkingCopy launchConfig = launchConfigType.newInstance(null, name);
		initLaunchConfiguration(launchConfig);

		if (webApplicationURL != null) {
			// Save the default URL of the web application in the configuration to change it later, for example if the application is
			// deployed with a different port.
			launchConfig.setAttribute(ATTR_ENV_VARIABLES, Map.of(ENV_VARIABLE_URL, webApplicationURL));
		}

		if (saveConfig)
			launchConfig.doSave();

		return new LaunchSettings(launchConfig, webApplicationURL, INITIAL_TIME_OUT, isDebugSupported());
	}

	/**
	 * Get the expression for the working directory based on the name of this configuration. It is assumed that the name of the
	 * configuration is equal to the name of the respective Eclipse project!
	 * @return the working directory
	 */
	protected String getWorkingDirectory() {
		return "${workspace_loc:/" + name + "}";
	}

	/**
	 * @return true if the application can be started in debug mode
	 */
	protected boolean isDebugSupported() {
		return true;
	}

	/**
	 * Search for an existing launch configuration by using the name and the type as filter criteria
	 * @return the existing launch configuration or null if it could not be found
	 * @throws CoreException if the launch configurations could not be retrieved
	 */
	private ILaunchConfiguration searchExistingLaunchConfiguration() throws CoreException {
		// Do not search for a configuration if it is just used temporarily (e.g. for building an application)!
		if (!saveConfig)
			return null;

		final ILaunchConfiguration[] launchConfigs = launchManager.getLaunchConfigurations();

		for (final ILaunchConfiguration config : launchConfigs)
			if (config.getName().equals(name) && config.getType().getIdentifier().equals(getLaunchConfigurationType()))
				return config;

		return null;
	}

	/**
	 * Search for the URL of the web application in the saved environment variables of the given launch configuration
	 * @param existingLaunchConfiguration
	 * @return the URL or null if it could not be found
	 * @throws CoreException if the configuration attribute for the URL doesn't contain a map
	 */
	private String getWebApplicationURL(ILaunchConfiguration existingLaunchConfiguration) throws CoreException {
		final Map<String, String> environmentVariables = existingLaunchConfiguration
				.getAttribute(AbstractLaunchSettingsGenerator.ATTR_ENV_VARIABLES, Collections.emptyMap());

		return environmentVariables.get(AbstractLaunchSettingsGenerator.ENV_VARIABLE_URL);
	}

}

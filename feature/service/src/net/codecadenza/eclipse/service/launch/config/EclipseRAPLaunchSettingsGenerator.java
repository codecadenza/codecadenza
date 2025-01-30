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

import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_PROGRAM_ARGUMENTS;
import static org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS;

import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.tools.util.eclipse.EclipseRAPPlugins;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/**
 * <p>
 * Create the launch settings for an Eclipse RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseRAPLaunchSettingsGenerator extends AbstractLaunchSettingsGenerator {
	private static final String ATTR_BROWSER_MODE = "org.eclipse.rap.launch.browserMode";
	private static final String ATTR_BUNDLES = "selected_target_bundles";
	private static final String ATTR_CONTEXT_PATH = "org.eclipse.rap.launch.contextpath";
	private static final String ATTR_OPEN_BROWSER = "org.eclipse.rap.launch.openBrowser";
	private static final String ATTR_SERVLET_PATH = "org.eclipse.rap.launch.servletPath";
	private static final String ATTR_WORKSPACE_BUNDLES = "selected_workspace_bundles";
	private static final String BROWSER_MODE = "EXTERNAL";
	private static final String CONFIG_TYPE = "org.eclipse.rap.ui.launch.RAPLauncher";
	private static final String CONTEXT_PATH = "/";
	private static final String PROGRAM_ARGUMENTS = "-os ${target.os} -ws ${target.ws} -arch ${target.arch} -nl ${target.nl} -console -consolelog";
	private static final String SERVLET_PATH = "/app";
	private static final String VM_ARGUMENTS = "-Declipse.ignoreApp=true -Dosgi.noShutdown=true -Dorg.eclipse.equinox.http.jetty.log.stderr.threshold=info";
	private static final String WORKSPACE_BUNDLE = "app.plugin.id@default:default";

	/**
	 * Constructor
	 * @param name the name of the launch configuration
	 * @param saveConfig flag that controls if a new launch configuration should be saved
	 */
	public EclipseRAPLaunchSettingsGenerator(String name, boolean saveConfig) {
		super(name, saveConfig, null);
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
		final Set<String> bundles = EclipseRAPPlugins.getPlugins().stream()
				.map(plugin -> plugin.name() + "@" + plugin.autoStartConfiguration()).collect(Collectors.toSet());
		final Set<String> workspaceBundles = Set.of(WORKSPACE_BUNDLE);

		launchConfig.setAttribute(ATTR_BROWSER_MODE, BROWSER_MODE);
		launchConfig.setAttribute(ATTR_BUNDLES, bundles);
		launchConfig.setAttribute(ATTR_CONTEXT_PATH, CONTEXT_PATH);
		launchConfig.setAttribute(ATTR_OPEN_BROWSER, true);
		launchConfig.setAttribute(ATTR_PROGRAM_ARGUMENTS, PROGRAM_ARGUMENTS);
		launchConfig.setAttribute(ATTR_SERVLET_PATH, SERVLET_PATH);
		launchConfig.setAttribute(ATTR_VM_ARGUMENTS, VM_ARGUMENTS);
		launchConfig.setAttribute(ATTR_WORKSPACE_BUNDLES, workspaceBundles);
	}

}

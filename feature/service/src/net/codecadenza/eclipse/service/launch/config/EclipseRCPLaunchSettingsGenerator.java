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

import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.tools.util.eclipse.EclipseRCPPlugins;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/**
 * <p>
 * Create the launch settings for an Eclipse RCP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseRCPLaunchSettingsGenerator extends AbstractLaunchSettingsGenerator {
	private static final String ATTR_APPLICATION = "application";
	private static final String ATTR_BUNDLES = "selected_target_bundles";
	private static final String ATTR_PRODUCT = "product";
	private static final String ATTR_PRODUCT_FILE = "productFile";
	private static final String ATTR_PRODUCT_NAME = "productName";
	private static final String ATTR_USE_PRODUCT = "useProduct";
	private static final String ATTR_WORKSPACE_BUNDLES = "selected_workspace_bundles";
	private static final String CONFIG_TYPE = "org.eclipse.pde.ui.RuntimeWorkbench";
	private static final String ECLIPSE_APPLICATION = "org.eclipse.e4.ui.workbench.swt.E4Application";
	private static final String PRODUCT_FILE_NAME = "application.product";
	private static final String PRODUCT_ID = "app.plugin.id.product";
	private static final String PRODUCT_NAME = "My application";
	private static final String WORKSPACE_BUNDLE = "app.plugin.id@default:default";

	/**
	 * Constructor
	 * @param projectName the name of the project or the name of the launch configuration
	 * @param saveConfig flag that controls if a new launch configuration should be saved
	 */
	public EclipseRCPLaunchSettingsGenerator(String projectName, boolean saveConfig) {
		super(projectName, null, saveConfig, null);
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
		final Set<String> bundles = EclipseRCPPlugins.getPlugins().stream()
				.map(plugin -> plugin.name() + "@" + plugin.autoStartConfiguration()).collect(Collectors.toSet());
		final Set<String> workspaceBundles = Set.of(WORKSPACE_BUNDLE);

		launchConfig.setAttribute(ATTR_APPLICATION, ECLIPSE_APPLICATION);
		launchConfig.setAttribute(ATTR_PRODUCT, PRODUCT_ID);
		launchConfig.setAttribute(ATTR_PRODUCT_FILE, "/" + projectName + "/" + PRODUCT_FILE_NAME);
		launchConfig.setAttribute(ATTR_PRODUCT_NAME, PRODUCT_NAME);
		launchConfig.setAttribute(ATTR_USE_PRODUCT, true);
		launchConfig.setAttribute(ATTR_BUNDLES, bundles);
		launchConfig.setAttribute(ATTR_WORKSPACE_BUNDLES, workspaceBundles);
	}

}

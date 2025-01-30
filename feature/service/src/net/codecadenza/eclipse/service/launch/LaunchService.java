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
package net.codecadenza.eclipse.service.launch;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.CodeCadenzaServicePlugin;
import net.codecadenza.eclipse.service.launch.config.LaunchSettings;
import net.codecadenza.eclipse.service.launch.config.LaunchSettingsGeneratorFactory;
import net.codecadenza.eclipse.service.launch.util.WebApplicationLaunchStatus;
import net.codecadenza.eclipse.service.launch.util.WebApplicationLauncher;
import net.codecadenza.eclipse.service.launch.util.WebApplicationMonitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchManager;

/**
 * <p>
 * Service for either running, debugging or building the applications of a project
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LaunchService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public LaunchService(Project project) {
		this.project = project;
	}

	/**
	 * Run the applications of the given project
	 * @throws CoreException if the launch settings could not be created
	 */
	public void runApplications() throws CoreException {
		final List<LaunchSettings> launchSettings = LaunchSettingsGeneratorFactory.createRunSettings(project);

		launchApplications(launchSettings, false, true);
	}

	/**
	 * Debug the applications of the given project
	 * @throws CoreException if the launch settings could not be created
	 */
	public void debugApplications() throws CoreException {
		final List<LaunchSettings> launchSettings = LaunchSettingsGeneratorFactory.createRunSettings(project);

		launchApplications(launchSettings, true, true);
	}

	/**
	 * Build the given project
	 * @throws IllegalStateException when trying to build a standalone Eclipse application
	 * @throws CoreException if the launch settings could not be created
	 */
	public void buildProject() throws CoreException {
		final List<LaunchSettings> launchSettings = LaunchSettingsGeneratorFactory.createBuildSettings(project);

		launchApplications(launchSettings, false, false);
	}

	/**
	 * Run the given test
	 * @param testToRun
	 * @throws CoreException if the launch settings could not be created
	 */
	public void runTest(JavaFile testToRun) throws CoreException {
		final LaunchSettings launchSettings = LaunchSettingsGeneratorFactory.createTestSettings(testToRun);

		launchSettings.launchConfiguration().launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());
	}

	/**
	 * Debug the given test
	 * @param testToRun
	 * @throws CoreException if the launch settings could not be created
	 */
	public void debugTest(JavaFile testToRun) throws CoreException {
		final LaunchSettings launchSettings = LaunchSettingsGeneratorFactory.createTestSettings(testToRun);

		launchSettings.launchConfiguration().launch(ILaunchManager.DEBUG_MODE, new NullProgressMonitor());
	}

	/**
	 * Launch the applications of the given project
	 * @param launchSettingsList a list with launch settings that should be launched
	 * @param debug flag that controls if the applications should be started in debug mode
	 * @param openInBrowser flag that controls if the web application should be opened in the browser
	 * @throws CoreException if an application could not be launched
	 */
	private void launchApplications(List<LaunchSettings> launchSettingsList, boolean debug, boolean openInBrowser)
			throws CoreException {
		final List<ILaunch> launchedApplications = new ArrayList<>();
		String url = null;
		Duration timeOut = null;

		for (final LaunchSettings launchSettings : launchSettingsList) {
			final String mode = debug && launchSettings.isDebugSupported() ? ILaunchManager.DEBUG_MODE : ILaunchManager.RUN_MODE;
			final ILaunch launch = launchSettings.launchConfiguration().launch(mode, new NullProgressMonitor());

			launchedApplications.add(launch);

			if (url == null)
				url = launchSettings.url();

			if (timeOut == null)
				timeOut = launchSettings.timeOut();
		}

		if (openInBrowser && url != null)
			launchWebApplicationInBrowser(launchedApplications, url, timeOut);
	}

	/**
	 * Launch the web application in the default browser
	 * @param launchedApplications a list with launched applications
	 * @param url the URL to be opened in the browser
	 * @param timeOut the maximum time to wait until the application is ready
	 */
	private void launchWebApplicationInBrowser(List<ILaunch> launchedApplications, String url, Duration timeOut) {
		final List<ILaunch> launchesToTrack;

		try {
			final URL webAppURL = URI.create(url).toURL();

			// Do not track whether a Jakarta EE deployment has been terminated. Otherwise, the URL will never be opened in the browser!
			if (project.isJakartaEEApplication())
				launchesToTrack = Collections.emptyList();
			else
				launchesToTrack = launchedApplications;

			final WebApplicationLaunchStatus status = new WebApplicationMonitor(webAppURL, launchesToTrack, timeOut).start();

			new WebApplicationLauncher(webAppURL, status).start();
		}
		catch (final MalformedURLException e) {
			CodeCadenzaServicePlugin.getDefault().getLog()
					.error("Error while opening the application in the browser. The URL '" + url + "' is invalid!", e);
		}
	}

}

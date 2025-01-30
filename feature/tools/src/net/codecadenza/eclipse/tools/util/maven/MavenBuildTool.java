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
package net.codecadenza.eclipse.tools.util.maven;

import java.util.Arrays;
import java.util.List;
import net.codecadenza.eclipse.shared.Constants;
import org.apache.maven.lifecycle.MavenExecutionPlan;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.embedder.IMaven;
import org.eclipse.m2e.core.project.IMavenProjectFacade;
import org.eclipse.m2e.core.project.IMavenProjectRegistry;

/**
 * </p>
 * Utility class for running Maven builds
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MavenBuildTool {
	private static final String GOAL_GENERATE_SOURCES = "generate-sources";

	/**
	 * Prevent instantiation
	 */
	private MavenBuildTool() {

	}

	/**
	 * Execute a Maven build with the goal 'generate-sources'
	 * @param wsProject the target workspace project
	 * @param monitor the progress monitor
	 * @throws CoreException if the Maven build has failed
	 */
	@SuppressWarnings("removal")
	public static void generateSources(IProject wsProject, IProgressMonitor monitor) throws CoreException {
		final IMaven maven = MavenPlugin.getMaven();
		final IMavenProjectRegistry projectRegistry = MavenPlugin.getMavenProjectRegistry();
		final IFile pomResource = wsProject.getFile(Constants.POM_XML);
		final IMavenProjectFacade facade = projectRegistry.create(pomResource, true, monitor);
		final MavenProject mavenProject = facade.getMavenProject(monitor);
		final MavenExecutionPlan plan = maven.calculateExecutionPlan(mavenProject, Arrays.asList(GOAL_GENERATE_SOURCES), true,
				monitor);
		final List<MojoExecution> mojos = plan.getMojoExecutions();

		for (final MojoExecution mojo : mojos)
			maven.execute(mavenProject, mojo, monitor);
	}
}

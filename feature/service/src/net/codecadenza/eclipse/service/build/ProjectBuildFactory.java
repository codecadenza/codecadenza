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
package net.codecadenza.eclipse.service.build;

import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.imp.EclipseProjectBuildService;
import net.codecadenza.eclipse.service.build.imp.MavenProjectBuildService;

/**
 * <p>
 * Factory for project build services
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectBuildFactory {
	/**
	 * Prevent instantiation
	 */
	private ProjectBuildFactory() {

	}

	/**
	 * @param project
	 * @return the project build service based on the given build tool
	 * @throws IllegalStateException if an implementation for the given build tool is not available
	 */
	public static IProjectBuildService getBuildService(Project project) {
		if (project.getBuildTool() == BuildToolEnumeration.MAVEN) {
			if (project.hasEclipseClient())
				return new EclipseProjectBuildService(project);

			return new MavenProjectBuildService(project);
		}

		throw new IllegalStateException("A build service for the build tool '" + project.getBuildTool() + "' is not available!");
	}

}

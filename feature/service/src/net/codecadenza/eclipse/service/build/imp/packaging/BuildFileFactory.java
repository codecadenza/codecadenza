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
package net.codecadenza.eclipse.service.build.imp.packaging;

import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.service.build.imp.packaging.imp.MavenBuildFileService;

/**
 * <p>
 * Factory for build file services
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BuildFileFactory {
	/**
	 * Prevent instantiation
	 */
	private BuildFileFactory() {

	}

	/**
	 * @param artifact
	 * @return the build file service due to the given build tool
	 * @throws IllegalStateException if a build file service for the given build tool doesn't exist
	 */
	public static IBuildFileService getBuildFileService(BuildArtifact artifact) {
		final BuildToolEnumeration buildTool = artifact.getProject().getBuildTool();

		if (buildTool == BuildToolEnumeration.MAVEN)
			return new MavenBuildFileService(artifact);

		throw new IllegalStateException("A build file service for the build tool '" + buildTool + "' is not available!");
	}

}

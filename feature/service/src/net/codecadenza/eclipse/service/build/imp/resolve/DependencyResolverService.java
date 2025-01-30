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
package net.codecadenza.eclipse.service.build.imp.resolve;

import static net.codecadenza.eclipse.shared.Constants.LIB_FOLDER;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.util.maven.MavenBuildTool;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Service for resolving all necessary dependencies of the given artifact
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DependencyResolverService {
	private static final String JAR_FILE_SUFFIX = ".jar";

	private final BuildArtifact artifact;
	private final Project project;

	/**
	 * Constructor
	 * @param artifact
	 */
	public DependencyResolverService(BuildArtifact artifact) {
		this.artifact = artifact;
		this.project = artifact.getProject();
	}

	/**
	 * Resolve all dependencies by running a respective Maven goal
	 * @param monitor
	 * @return a list with all classpath entries
	 * @throws Exception if the dependencies could not be resolved
	 */
	public List<IClasspathEntry> resolveDependencies(IProgressMonitor monitor) throws Exception {
		final WorkspaceFile buildFile = new MavenResolverBuildFileService(artifact).createBuildFile();

		// If no build file has been returned the dependencies don't have to be resolved!
		if (buildFile == null)
			return Collections.emptyList();

		final var libPath = "/" + project.getTargetProjectName(BuildArtifactType.GUI) + "/" + LIB_FOLDER + "/";
		final var entries = new ArrayList<IClasspathEntry>();

		// Create the build file
		EclipseIDEService.createOrUpdateFile(buildFile);

		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject wsProject = wsRoot.getProject(project.getTargetProjectName(artifact.getType()));

		// Get the dependencies by running a Maven build
		MavenBuildTool.generateSources(wsProject, monitor);

		// Delete the build file as it is no longer necessary
		EclipseIDEService.deleteWorkspaceFile(buildFile);

		// Refresh the library folder
		wsProject.refreshLocal(IResource.DEPTH_INFINITE, monitor);

		for (final IResource resource : wsProject.getFolder(Constants.LIB_FOLDER).members())
			if (resource.getLocation().toString().endsWith(JAR_FILE_SUFFIX))
				entries.add(JavaCore.newLibraryEntry(new Path(libPath + resource.getName()), null, null));

		return entries;
	}
}

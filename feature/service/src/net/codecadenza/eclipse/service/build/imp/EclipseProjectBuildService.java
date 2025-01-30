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
package net.codecadenza.eclipse.service.build.imp;

import static net.codecadenza.eclipse.shared.Constants.LIB_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.LIB_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.META_INF_FOLDER;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.generator.basic.client.imp.EclipseClientProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Project build service for Eclipse RCP/RAP projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseProjectBuildService extends MavenProjectBuildService {
	/**
	 * Constructor
	 * @param project
	 */
	public EclipseProjectBuildService(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.MavenProjectBuildService#getDefaultBuildConfiguration()
	 */
	@Override
	public List<BuildArtifact> getDefaultBuildConfiguration() {
		if (project.isJavaSEApplication()) {
			final var buildConfig = new ArrayList<BuildArtifact>();
			final BuildArtifact guiArtifact = createDefaultRootArtifact(BuildArtifactType.GUI);

			buildConfig.add(guiArtifact);

			guiArtifact.getContainedArtifacts().add(BuildArtifactType.DOMAIN);
			guiArtifact.getContainedArtifacts().add(BuildArtifactType.DATA_EXCHANGE);
			guiArtifact.getContainedArtifacts().add(BuildArtifactType.DTO);
			guiArtifact.getContainedArtifacts().add(BuildArtifactType.SHARED);

			if (project.isBoundaryMode()) {
				guiArtifact.getContainedArtifacts().add(BuildArtifactType.REPOSITORY);
				guiArtifact.getContainedArtifacts().add(BuildArtifactType.BOUNDARY);
				guiArtifact.getContainedArtifacts().add(BuildArtifactType.CLIENT_INTERFACE);
			}
			else
				guiArtifact.getContainedArtifacts().add(BuildArtifactType.FACADE);

			return buildConfig;
		}

		return super.getDefaultBuildConfiguration();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.base.AbstractProjectBuildService#getInternalLibraries(net.codecadenza.eclipse.
	 * model.project.BuildArtifact, org.eclipse.core.resources.IProject)
	 */
	@Override
	protected HashSet<IClasspathEntry> getInternalLibraries(BuildArtifact artifact, IProject wsProject) throws Exception {
		if (artifact.getType() != BuildArtifactType.GUI)
			return new HashSet<>();

		final Set<String> clientLibFiles = importClientLibFiles(wsProject, null);
		final var classPathEntries = new HashSet<IClasspathEntry>();
		final var libPath = "/" + project.getTargetProjectName(BuildArtifactType.GUI) + "/" + LIB_FOLDER + "/";

		clientLibFiles.forEach(libName -> classPathEntries.add(JavaCore.newLibraryEntry(new Path(libPath + libName), null, null)));

		return classPathEntries;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.base.AbstractProjectBuildService#createCustomBuildFiles(net.codecadenza.
	 * eclipse.model.project.BuildArtifact, org.eclipse.core.resources.IProject, java.util.HashSet)
	 */
	@Override
	protected void createCustomBuildFiles(BuildArtifact artifact, IProject wsProject, HashSet<IClasspathEntry> classPathEntries)
			throws Exception {
		if (artifact.getType() != BuildArtifactType.GUI)
			return;

		final var eclipseGenerator = new EclipseClientProjectFilesGenerator(project);
		String content = eclipseGenerator.createManifest(wsProject, project.getClientNamespace().toString());

		EclipseIDEService
				.createOrUpdateFile(new WorkspaceFile(project, BuildArtifactType.GUI, META_INF_FOLDER + "/MANIFEST.MF", content));

		content = eclipseGenerator.createBuildPropertiesFile(wsProject);

		EclipseIDEService.createOrUpdateFile(new WorkspaceFile(project, BuildArtifactType.GUI, "build.properties", content));
	}

	/**
	 * Import additional libraries for GUI artifacts
	 * @param workspaceProject
	 * @param monitor
	 * @return a set containing all files to be imported
	 * @throws Exception if the import operation has failed
	 */
	private Set<String> importClientLibFiles(IProject workspaceProject, IProgressMonitor monitor) throws Exception {
		final var libFiles = new HashSet<String>();

		if (!project.isJavaSEApplication()) {
			// Import the dummy library. This library is empty but it is used as a blueprint for internal project dependencies!
			final String dummyLibName = importFileFromURL(workspaceProject, CodeCadenzaResourcePlugin.getDummyLibrary(), monitor);

			// In case of an Eclipse RCP/RAP project, empty libraries must be provided, which will be replaced when the server artifact
			// is created
			final IFolder libFolder = workspaceProject.getFolder(LIB_FOLDER);
			final IFile dummyLibFile = libFolder.getFile(dummyLibName);
			final String apiLibName = project.getTargetProjectName(BuildArtifactType.CLIENT_INTERFACE) + LIB_SUFFIX;
			final String dtoLibName = project.getTargetProjectName(BuildArtifactType.DTO) + LIB_SUFFIX;
			final String sharedLibName = project.getTargetProjectName(BuildArtifactType.SHARED) + LIB_SUFFIX;

			dummyLibFile.copy(libFolder.getFullPath().append(apiLibName), true, monitor);
			dummyLibFile.copy(libFolder.getFullPath().append(dtoLibName), true, monitor);
			dummyLibFile.copy(libFolder.getFullPath().append(sharedLibName), true, monitor);

			libFiles.add(apiLibName);
			libFiles.add(dtoLibName);
			libFiles.add(sharedLibName);

			// Delete the dummy library as it is no longer necessary
			dummyLibFile.delete(true, monitor);
		}

		return libFiles;
	}

	/**
	 * Import a file from the given URI into the project's library folder
	 * @param project
	 * @param uri
	 * @param monitor
	 * @return the name of the imported library
	 * @throws Exception if the import operation has failed
	 */
	private String importFileFromURL(IProject project, URI uri, IProgressMonitor monitor) throws Exception {
		final String fileName = uri.toURL().getFile().substring(uri.toURL().getFile().lastIndexOf('/') + 1);
		final IFile importFile = project.getFile(LIB_FOLDER + "/" + fileName);

		if (!importFile.exists())
			importFile.create(uri.toURL().openStream(), true, monitor);

		return fileName;
	}

}

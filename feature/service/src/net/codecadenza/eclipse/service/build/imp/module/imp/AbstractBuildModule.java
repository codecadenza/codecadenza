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
package net.codecadenza.eclipse.service.build.imp.module.imp;

import static net.codecadenza.eclipse.shared.Constants.CONT_PATH_JUNIT;
import static net.codecadenza.eclipse.shared.Constants.CONT_PATH_MAVEN;
import static net.codecadenza.eclipse.shared.Constants.JAVA_NATURE_ID;
import static net.codecadenza.eclipse.shared.Constants.MAVEN_NATURE_ID;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.generator.basic.project.ProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.service.build.imp.module.IBuildModule;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Abstract base class for all build modules
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractBuildModule implements IBuildModule {
	protected static final String BEANS_XML = "beans.xml";
	protected static final String TEST_ATTRIBUTE = "test";
	protected static final String TEST_CLASSES_TARGET_PATH = "/target/test-classes";

	protected Project project;
	protected BuildArtifact buildArtifact;

	/**
	 * Constructor
	 * @param buildArtifact
	 */
	protected AbstractBuildModule(BuildArtifact buildArtifact) {
		this.buildArtifact = buildArtifact;
		this.project = buildArtifact.getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.IBuildModule#getNatures()
	 */
	@Override
	public List<String> getNatures() {
		final var natures = new ArrayList<String>();
		natures.add(JAVA_NATURE_ID);
		natures.add(MAVEN_NATURE_ID);

		return natures;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.IBuildModule#getCompilerOptions()
	 */
	@Override
	public Map<String, String> getCompilerOptions() {
		final var options = new HashMap<String, String>();
		options.put(JavaCore.COMPILER_COMPLIANCE, JavaCore.VERSION_21);
		options.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_21);
		options.put(JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM, JavaCore.VERSION_21);

		return options;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.module.IBuildModule#getConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> getConfigurationFiles() {
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.IBuildModule#getPackages()
	 */
	@Override
	public List<String> getPackages() {
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.IBuildModule#getTestPackages()
	 */
	@Override
	public List<String> getTestPackages() {
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.IBuildModule#createInitialSourceFiles()
	 */
	@Override
	public void createInitialSourceFiles() throws Exception {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.IBuildModule#getFolders()
	 */
	@Override
	public List<String> getFolders() {
		final var folderNames = new ArrayList<String>();
		folderNames.add(project.getSourceFolder());
		folderNames.add(project.getResourceFolder());
		folderNames.add(project.getTestSourceFolder());

		return folderNames;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.IBuildModule#getClassPathEntries(java.lang.String)
	 */
	@Override
	public HashSet<IClasspathEntry> getClassPathEntries(String projectName) {
		final var classPathEntries = new HashSet<IClasspathEntry>();
		classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getSourceFolder())));
		classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getResourceFolder())));
		classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getTestSourceFolder())));
		classPathEntries.add(JavaCore.newContainerEntry(new Path(CONT_PATH_MAVEN)));
		classPathEntries.add(JavaCore.newContainerEntry(new Path(CONT_PATH_JUNIT)));

		return classPathEntries;
	}

	/**
	 * Add the beans.xml file to the configuration file list
	 * @param configFiles
	 * @param artifactType
	 */
	protected void addBeansXML(List<WorkspaceFile> configFiles, BuildArtifactType artifactType) {
		// Configuration files should be added only if this build artifact is not contained in other artifacts!
		for (final BuildArtifact artifact : project.getBuildConfiguration())
			if (artifact.getContainedArtifacts().contains(artifactType))
				return;

		if (!addBeansXML(artifactType))
			return;

		final var path = project.getMetaInfFolder() + "/" + BEANS_XML;
		final String content = new ProjectFilesGenerator(project).createBeansXML();

		final var beansXmlFile = new WorkspaceFile(buildArtifact.getName(), BEANS_XML, path);
		beansXmlFile.setContent(content);

		configFiles.add(beansXmlFile);
	}

	/**
	 * @param artifactType
	 * @return true if the beans.xml file should be added
	 */
	protected boolean addBeansXML(BuildArtifactType artifactType) {
		if (project.isJakartaEEApplication())
			return true;

		// Integration client artifacts should always provide a beans.xml file!
		return artifactType == BuildArtifactType.INTEGRATION_CLIENT_REST || artifactType == BuildArtifactType.INTEGRATION_CLIENT_SOAP
				|| artifactType == BuildArtifactType.INTEGRATION_CLIENT_RMI || artifactType == BuildArtifactType.INTEGRATION_CLIENT_KAFKA;
	}

}

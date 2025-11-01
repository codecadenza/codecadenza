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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.generator.basic.testing.ITestingProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.testing.TestingProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.generator.service.LoggingConfigurationGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Build module for integration tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestBuildModule extends AbstractBuildModule {
	private final AbstractTestModule testModule;
	private final ITestingProjectFilesGenerator testingProjectFilesGenerator;

	/**
	 * Constructor
	 * @param buildArtifact
	 * @param artifactType
	 */
	public IntegrationTestBuildModule(BuildArtifact buildArtifact, BuildArtifactType artifactType) {
		super(buildArtifact);

		this.testModule = project.getTestModuleByArtifact(artifactType);
		this.testingProjectFilesGenerator = TestingProjectFilesGeneratorFactory.getGenerator(testModule, artifactType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getFolders()
	 */
	@Override
	public List<String> getFolders() {
		return Arrays.asList(project.getTestSourceFolder(), project.getTestDataFolder());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> getConfigurationFiles() {
		final String loggingConfigPath = project.getTestResourceFolder() + "/log4j2.xml";
		final String content = new LoggingConfigurationGenerator(buildArtifact).createContent();

		// Create the logging configuration file
		final var loggingConfFile = new WorkspaceFile(project, buildArtifact.getType(), loggingConfigPath, content);

		final List<WorkspaceFile> configFiles = new ArrayList<>(testingProjectFilesGenerator.createConfigurationFiles());
		configFiles.add(loggingConfFile);

		return configFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getClassPathEntries(java.lang.String)
	 */
	@Override
	public HashSet<IClasspathEntry> getClassPathEntries(String projectName) {
		final IClasspathAttribute[] testAttributes = { JavaCore.newClasspathAttribute(TEST_ATTRIBUTE, Boolean.TRUE.toString()) };
		final Path testSourcePath = new Path("/" + projectName + "/" + project.getTestSourceFolder());
		final Path testTargetPath = new Path("/" + projectName + TEST_CLASSES_TARGET_PATH);

		final var classPathEntries = new HashSet<IClasspathEntry>();
		classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getTestResourceFolder())));
		classPathEntries.add(JavaCore.newSourceEntry(testSourcePath, null, null, testTargetPath, testAttributes));
		classPathEntries.add(JavaCore.newContainerEntry(new Path(CONT_PATH_MAVEN)));
		classPathEntries.add(JavaCore.newContainerEntry(new Path(CONT_PATH_JUNIT), null, testAttributes, false));

		return classPathEntries;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getTestPackages()
	 */
	@Override
	public List<String> getTestPackages() {
		return List.of(testModule.getNamespace().toString());
	}

}

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
package net.codecadenza.eclipse.generator.basic.testing.imp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.generator.basic.testing.ITestingProjectFilesGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;

/**
 * <p>
 * Generator for basic configuration files that are necessary for integration tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestProjectFilesGenerator implements ITestingProjectFilesGenerator {
	private final IntegrationTestModule testModule;
	private final BuildArtifactType artifactType;
	private final Project project;

	/**
	 * Constructor
	 * @param testModule
	 * @param artifactType
	 */
	public IntegrationTestProjectFilesGenerator(IntegrationTestModule testModule, BuildArtifactType artifactType) {
		this.testModule = testModule;
		this.artifactType = artifactType;
		this.project = testModule.getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.testing.ITestingProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.testing.ITestingProjectFilesGenerator#createConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> createConfigurationFiles() {
		final var testPropertiesPath = project.getTestResourceFolder() + "/test.properties";

		final var fileList = new ArrayList<WorkspaceFile>();
		fileList.add(new WorkspaceFile(project, artifactType, testPropertiesPath, createTestProperties()));

		return fileList;
	}

	/**
	 * @return the content of the test.properties file
	 */
	private String createTestProperties() {
		final var b = new StringBuilder();
		b.append("TEST_DATA_PROVIDER_CLASS_NAME=net.codecadenza.runtime.ddt.service.data.imp.xml.XMLTestDataProvider\n");
		b.append("INVOCATION_HANDLER_CLASS_NAME=");
		b.append("net.codecadenza.runtime.ddt.service.completion.imp.JDBCInvocationCompletionHandler\n");
		b.append("STATEMENT_PROCESSOR_CLASS_NAME=net.codecadenza.runtime.ddt.service.preparation.imp.JDBCStatementProcessor\n");
		b.append("DEFAULT_TIMEOUT_MS=" + testModule.getDefaultTimeout() + "\n");
		b.append("DATE_FORMAT=" + testModule.getDateFormat() + "\n");
		b.append("DATE_TIME_FORMAT=" + testModule.getDateTimeFormat() + "\n");
		b.append("DECIMAL_FORMAT=" + testModule.getDecimalFormat() + "\n");
		b.append("DECIMAL_SEPARATOR=" + testModule.getDecimalSeparator() + "\n");
		b.append("GROUPING_SEPARATOR=" + testModule.getGroupingSeparator() + "\n");
		b.append("RESOURCE_URL=" + testModule.getProject().getDataSource().getConnectionURL() + "\n");
		b.append("USER_NAME=" + testModule.getProject().getDataSource().getUserName() + "\n");
		b.append("PASSWORD=" + testModule.getProject().getDataSource().getPassword() + "\n");

		return b.toString();
	}

}

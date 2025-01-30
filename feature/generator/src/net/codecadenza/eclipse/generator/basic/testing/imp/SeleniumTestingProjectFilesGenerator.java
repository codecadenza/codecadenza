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

import static net.codecadenza.eclipse.shared.Constants.INDEX_PAGE_NAME;
import static net.codecadenza.eclipse.shared.Constants.LOGIN_PAGE_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_PAGE_OBJECT;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.generator.basic.testing.ITestingProjectFilesGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaTestFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.SeleniumDriver;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;

/**
 * <p>
 * Generator for basic source and configuration files that are necessary for Selenium tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumTestingProjectFilesGenerator implements ITestingProjectFilesGenerator {
	private final SeleniumTestModule testModule;
	private final BuildArtifactType artifactType;
	private final Project project;

	/**
	 * Constructor
	 * @param testModule
	 * @param artifactType
	 */
	public SeleniumTestingProjectFilesGenerator(SeleniumTestModule testModule, BuildArtifactType artifactType) {
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
		final var indexPage = new JavaTestFile(project, BuildArtifactType.SELENIUM_TEST, INDEX_PAGE_NAME,
				testModule.getNamespace().toString() + PACK_PAGE_OBJECT);
		indexPage.setComment("Page object that represents the index page");
		indexPage.setContent(createIndexPage());

		final var loginPage = new JavaTestFile(project, BuildArtifactType.SELENIUM_TEST, LOGIN_PAGE_NAME,
				testModule.getNamespace().toString() + PACK_PAGE_OBJECT);
		loginPage.setComment("Page object that represents the login page");
		loginPage.setContent(createLoginPage());

		final var sourceFiles = new ArrayList<JavaFile>();
		sourceFiles.add(indexPage);
		sourceFiles.add(loginPage);

		return sourceFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.testing.ITestingProjectFilesGenerator#createConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> createConfigurationFiles() {
		final var fileList = new ArrayList<WorkspaceFile>();
		final var testPropertiesPath = project.getTestResourceFolder() + "/test.properties";

		fileList.add(new WorkspaceFile(project, artifactType, testPropertiesPath, createTestProperties()));

		return fileList;
	}

	/**
	 * Create the index page
	 * @return the generated content
	 */
	private String createIndexPage() {
		final var b = new StringBuilder();
		b.append("import net.codecadenza.runtime.selenium.junit.*;\n");

		if (project.hasAngularClient())
			b.append("import net.codecadenza.runtime.selenium.page.imp.angular.*;\n");
		else if (project.hasJSFClient())
			b.append("import net.codecadenza.runtime.selenium.page.imp.primefaces.*;\n");
		else
			b.append("import net.codecadenza.runtime.selenium.page.imp.vaadin.*;\n");

		b.append("\n");
		b.append("public class " + INDEX_PAGE_NAME + " extends AbstractPageObject\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param testContext\n");
		b.append(" */\n");
		b.append("public " + INDEX_PAGE_NAME + "(SeleniumTestContext testContext)\n");
		b.append("{\n");
		b.append("super(testContext);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the login page
	 * @return the generated content
	 */
	private String createLoginPage() {
		final var b = new StringBuilder();
		b.append("import net.codecadenza.runtime.selenium.data.*;\n");
		b.append("import net.codecadenza.runtime.selenium.junit.*;\n");

		if (project.hasAngularClient())
			b.append("import net.codecadenza.runtime.selenium.page.imp.angular.*;\n");
		else if (project.hasJSFClient())
			b.append("import net.codecadenza.runtime.selenium.page.imp.primefaces.*;\n");
		else
			b.append("import net.codecadenza.runtime.selenium.page.imp.vaadin.*;\n");

		b.append("\n");
		b.append("public class " + LOGIN_PAGE_NAME + " extends AbstractPageObject\n");
		b.append("{\n");
		b.append("private static final int TEST_DATA_COUNT = 2;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param testContext\n");
		b.append(" */\n");
		b.append("public " + LOGIN_PAGE_NAME + "(SeleniumTestContext testContext)\n");
		b.append("{\n");
		b.append("super(testContext);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Perform user login\n");
		b.append(" * @param testData\n");
		b.append(" * @return an instance of the index page\n");
		b.append(" * @throws AssertionError if the test data is invalid\n");
		b.append(" */\n");
		b.append("public " + INDEX_PAGE_NAME + " login(PageActionTestData testData)\n");
		b.append("{\n");
		b.append("if(testData.getElementTestData().size() != TEST_DATA_COUNT)\n");
		b.append("fail(\"Test data for performing login is either missing or incomplete!\");\n\n");
		b.append("setInputFieldValue(testData.getElementTestData().get(0));\n");
		b.append("setInputFieldValue(testData.getElementTestData().get(1));\n\n");
		b.append("pressButton(BUTTON_ID_LOG_IN);\n\n");
		b.append("return createPageObject(" + INDEX_PAGE_NAME + ".class);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the content of the test.properties file
	 */
	private String createTestProperties() {
		final var b = new StringBuilder();
		final String driverName;
		final String driverClass;

		if (project.hasAngularClient())
			b.append("BASE_URL=http://localhost:4200\n");
		else
			b.append("BASE_URL=http://localhost:8080/" + project.getCode() + "\n");

		b.append("# The time in seconds to wait for a page load to complete\n");
		b.append("PAGE_LOAD_TIMEOUT=" + testModule.getPageLoadTime() + "\n");
		b.append("# The time in seconds the driver should wait when searching for an element if it is not immediately present\n");
		b.append("IMPLICIT_WAIT_TIME=" + testModule.getImplicitWaitTime() + "\n");

		if (testModule.getDriverPath() != null && !testModule.getDriverPath().isEmpty())
			b.append("DRIVER_PATH=" + testModule.getDriverPath() + "\n");

		if (testModule.getDriver() == SeleniumDriver.CHROME) {
			driverName = "webdriver.chrome.driver";
			driverClass = "org.openqa.selenium.chrome.ChromeDriver";
		}
		else if (testModule.getDriver() == SeleniumDriver.EDGE) {
			driverName = "webdriver.edge.driver";
			driverClass = "org.openqa.selenium.edge.EdgeDriver";
		}
		else if (testModule.getDriver() == SeleniumDriver.FIREFOX) {
			driverName = "webdriver.firefox.driver";
			driverClass = "org.openqa.selenium.firefox.FirefoxDriver";
		}
		else if (testModule.getDriver() == SeleniumDriver.INTERNET_EXPLORER) {
			driverName = "webdriver.internetexplorer.driver";
			driverClass = "org.openqa.selenium.ie.InternetExplorerDriver";
		}
		else if (testModule.getDriver() == SeleniumDriver.OPERA) {
			driverName = "webdriver.opera.driver";
			driverClass = "org.openqa.selenium.opera.OperaDriver";
		}
		else {
			driverName = "webdriver.safari.driver";
			driverClass = "org.openqa.selenium.safari.SafariDriver";
		}

		b.append("DRIVER_CLASS=" + driverClass + "\n");
		b.append("DRIVER_NAME=" + driverName + "\n");
		b.append("DRIVER_MAXIMIZE_WINDOW=" + testModule.isMaximizeWindow() + "\n");
		b.append("TEST_DATA_PATH=" + project.getTestDataFolder() + "\n");
		b.append("DATA_PROVIDER_CLASS=net.codecadenza.runtime.selenium.data.imp.XMLFileTestDataProvider\n");
		b.append("DRIVER_PRODUCER_CLASS=net.codecadenza.runtime.selenium.driver.imp.StandardDriverProducer\n");
		b.append("# The time in seconds to wait until all pending HTTP requests should have been finished\n");
		b.append("HTTP_WAIT_TIMEOUT=5\n");
		b.append("# The time in milliseconds to wait before checking for pending HTTP requests\n");

		if (project.hasVaadinClient())
			b.append("HTTP_CHECK_DELAY=300\n");
		else
			b.append("HTTP_CHECK_DELAY=0\n");

		b.append("# The time in milliseconds for delaying a test after creating a page object\n");

		if (project.hasVaadinClient())
			b.append("PAGE_LOAD_DELAY=100\n");
		else
			b.append("PAGE_LOAD_DELAY=0\n");

		return b.toString();
	}

}

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
package net.codecadenza.eclipse.service.build.imp.packaging.imp;

import static net.codecadenza.eclipse.shared.Constants.JAVAFX_LAUNCHER_NAME;
import static net.codecadenza.eclipse.shared.Constants.LIB_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.LIB_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.service.build.imp.base.AbstractMavenBuildFileService;
import net.codecadenza.eclipse.service.build.imp.packaging.IBuildFileService;
import net.codecadenza.eclipse.shared.Constants;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Parent;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Build file service for Maven
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MavenBuildFileService extends AbstractMavenBuildFileService implements IBuildFileService {
	private static final String MAVEN_PLUGIN_GROUP = "org.apache.maven.plugins";

	// Constants for Payara
	private static final String GL_PLUGIN_GROUP = "org.glassfish.maven.plugin";
	private static final String GL_PLUGIN_VERSION = "2.1";
	private static final String GL_PLUGIN_ID = "maven-glassfish-plugin";
	private static final String GL_ELEMENT_ARTIFACT = "artifact";
	private static final String GL_ELEMENT_COMPONENT = "component";
	private static final String GL_ELEMENT_COMPONENTS = "components";
	private static final String GL_ELEMENT_COMPONENT_NAME = "name";
	private static final String GL_ELEMENT_ADMIN_PORT = "adminPort";
	private static final String GL_ELEMENT_HTTP_PORT = "httpPort";
	private static final String GL_ELEMENT_DOMAIN_NAME = "name";
	private static final String GL_ELEMENT_DOMAIN = "domain";
	private static final String GL_ELEMENT_ECHO = "echo";
	private static final String GL_ELEMENT_TERSE = "terse";
	private static final String GL_ELEMENT_DEBUG = "debug";
	private static final String GL_ELEMENT_PASSWORD = "adminPassword";
	private static final String GL_ELEMENT_USER = "user";
	private static final String GL_ELEMENT_DIRECTORY = "glassfishDirectory";

	private static final String GL_VALUE_COMPONENT_ARTIFACT = "target/${project.build.finalName}.war";
	private static final String GL_VALUE_ADMIN_PORT = "4848";
	private static final String GL_VALUE_HTTP_PORT = "8080";
	private static final String GL_VALUE_DOMAIN = "domain1";
	private static final String GL_VALUE_TERSE = "false";
	private static final String GL_VALUE_DEBUG = "true";

	// Constants for the Maven WAR plug-in
	private static final String MAVEN_WAR_PLUGIN_ID = "maven-war-plugin";
	private static final String MAVEN_WAR_PLUGIN_VERSION = "3.5.1";

	// Constants for Wildfly
	private static final String WILDFLY_PLUGIN_GROUP = "org.wildfly.plugins";
	private static final String WILDFLY_PLUGIN_VERSION = "5.1.5.Final";
	private static final String WILDFLY_PLUGIN_ID = "wildfly-maven-plugin";

	// Constants for Vaadin
	private static final String VD_VERSION = "25.0.2";
	private static final String VD_PLUGIN_GROUP = "com.vaadin";
	private static final String VD_PLUGIN_ID = "vaadin-maven-plugin";
	private static final String VD_ARTIFACT_ID_BOM = "vaadin-bom";
	private static final String VD_GOAL_PREPARE_FRONTEND = "prepare-frontend";

	// Constants for Avro
	private static final String AVRO_PLUGIN_GROUP = "org.apache.avro";
	private static final String AVRO_PLUGIN_ID = "avro-maven-plugin";
	private static final String AVRO_PLUGIN_VERSION = "1.12.1";
	private static final String AVRO_GOAL_IDL = "idl-protocol";
	private static final String AVRO_ELEMENT_ST = "stringType";
	private static final String AVRO_ELEMENT_DT = "enableDecimalLogicalType";
	private static final String AVRO_ELEMENT_FV = "fieldVisibility";
	private static final String AVRO_ELEMENT_SD = "sourceDirectory";
	private static final String AVRO_ELEMENT_OD = "outputDirectory";

	private static final String AVRO_VALUE_ST = "String";
	private static final String AVRO_VALUE_DT = "true";
	private static final String AVRO_VALUE_FV = "private";
	private static final String AVRO_VALUE_SD = "${project.basedir}/src/main/resources";
	private static final String AVRO_VALUE_OD = "${project.basedir}/src/main/java";

	// Constants for the dependency plug-in
	private static final String DEP_PLUGIN_ID = "maven-dependency-plugin";
	private static final String DEP_ELEMENT_ITEMS = "artifactItems";
	private static final String DEP_ELEMENT_ITEM = "artifactItem";
	private static final String DEP_ELEMENT_FILE = "destFileName";
	private static final String DEP_ELEMENT_DIR = "outputDirectory";

	// Constants for the assembly plug-in
	private static final String ASSEMBLY_PLUGIN_ID = "maven-assembly-plugin";
	private static final String ASSEMBLY_PLUGIN_VERSION = "3.7.1";
	private static final String ASSEMBLY_GOAL = "single";
	private static final String ASSEMBLY_EXECUTION_ID = "make-assembly";
	private static final String ASSEMBLY_ELEMENT_REFS = "descriptorRefs";
	private static final String ASSEMBLY_ELEMENT_REF = "descriptorRef";
	private static final String ASSEMBLY_VALUE_REF = "jar-with-dependencies";
	private static final String ASSEMBLY_ELEMENT_ARCHIVE = "archive";
	private static final String ASSEMBLY_ELEMENT_MANIFEST = "manifest";
	private static final String ASSEMBLY_ELEMENT_MAIN_CLASS = "mainClass";

	// Names of the main classes for Swing and JavaFx
	private static final String MAIN_CLASS_SWING = ".Application";

	// Constants for Spring Boot
	private static final String SB_GROUP = "org.springframework.boot";
	private static final String SB_VERSION = "4.0.1";
	private static final String SB_MAVEN_PLUGIN_ID = "spring-boot-maven-plugin";
	private static final String SB_PARENT_ARTIFACT_ID = "spring-boot-starter-parent";

	// Default server authentication values
	private static final String DEFAULT_USER = "admin";
	private static final String DEFAULT_PASSWORD = "admin";

	private static final String WILDFLY_ELEMENT_PASSWORD = "password";
	private static final String WILDFLY_ELEMENT_USER = "username";
	private static final String WILDFLY_ELEMENT_DIRECTORY = "jbossHome";
	private static final String WILDFLY_ELEMENT_NAME = "name";

	private static final String PROP_NAME_ENCODING = "project.build.sourceEncoding";
	private static final String PROP_NAME_COMP_SOURCE = "maven.compiler.source";
	private static final String PROP_NAME_COMP_TARGET = "maven.compiler.target";

	/**
	 * Constructor
	 * @param artifact
	 */
	public MavenBuildFileService(BuildArtifact artifact) {
		super(artifact);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.packaging.IBuildFileService#createBuildFile(java.lang.String)
	 */
	@Override
	public WorkspaceFile createBuildFile(String serverDirectory) throws Exception {
		if (artifact.getType() == BuildArtifactType.GUI && (project.hasAngularClient() || project.hasEclipseClient()))
			return null;

		// Determine if this artifact requires a parent
		boolean isParentRequired = project.getBuildConfiguration().size() > 1;

		// A Selenium artifact that is part of a Spring Boot project must not have a parent! Otherwise, Maven pulls
		// library versions that are defined in the spring-boot-dependencies pom.xml file. It is very likely that
		// these versions do not match the versions required by CodeCadenza's Selenium runtime library!
		if (project.isSpringBootApplication() && (artifact.getType() == BuildArtifactType.SELENIUM_TEST
				|| (artifact.getType() == BuildArtifactType.GUI && (project.hasJavaFXClient() || project.hasSwingClient()))))
			isParentRequired = false;

		if (isParentRequired && artifact.getType() != BuildArtifactType.MASTER) {
			final var parent = new Parent();
			parent.setGroupId(project.getRootNamespace().toString());

			for (final BuildArtifact buildArtifact : project.getBuildConfiguration())
				if (buildArtifact.getType() == BuildArtifactType.MASTER) {
					parent.setArtifactId(buildArtifact.getName());
					break;
				}

			parent.setVersion(DEFAULT_VERSION);

			mavenModel.setParent(parent);
		}
		else {
			mavenModel.setGroupId(project.getRootNamespace().toString());
			mavenModel.setVersion(DEFAULT_VERSION);
		}

		mavenModel.setArtifactId(artifact.getName());

		if (artifact.getType() == BuildArtifactType.MASTER)
			configureMasterArtifact();
		else if (artifact.getType() == BuildArtifactType.SERVER)
			mavenModel.setPackaging(PACKAGING_TYPE_WAR);
		else if (artifact.getType() == BuildArtifactType.GUI)
			configureGUIArtifact();
		else
			mavenModel.setPackaging(PACKAGING_TYPE_JAR);

		mavenModel.setDependencies(getDependencies());

		if (artifact.getType() == BuildArtifactType.MASTER || !isParentRequired) {
			mavenModel.getProperties().put(PROP_NAME_ENCODING, UTF_8);
			mavenModel.getProperties().put(PROP_NAME_COMP_SOURCE, JavaCore.VERSION_25);
			mavenModel.getProperties().put(PROP_NAME_COMP_TARGET, JavaCore.VERSION_25);
		}

		if (artifact.getType() != BuildArtifactType.MASTER)
			addPlugins(serverDirectory);

		final var sw = new StringWriter();

		// Generate the content of the pom.xml file
		final var writer = new MavenXpp3Writer();
		writer.write(sw, mavenModel);

		return new WorkspaceFile(project, artifact.getType(), Constants.POM_XML, sw.toString());
	}

	/**
	 * Configure the Maven model for a master artifact
	 */
	private void configureMasterArtifact() {
		mavenModel.setPackaging(PACKAGING_TYPE_POM);

		if (project.isSpringBootApplication()) {
			final var parent = new Parent();
			parent.setGroupId(SB_GROUP);
			parent.setArtifactId(SB_PARENT_ARTIFACT_ID);
			parent.setVersion(SB_VERSION);

			mavenModel.setParent(parent);
		}

		// Sort modules so that test modules are added after non-test modules
		final List<BuildArtifact> sortedArtifacts = new ArrayList<>(project.getBuildConfiguration());
		sortedArtifacts.sort(Comparator.comparingInt(artifact -> artifact.isTestArtifact() ? 1 : 0));

		for (final BuildArtifact buildArtifact : sortedArtifacts) {
			if (buildArtifact.getType() == BuildArtifactType.MASTER)
				continue;

			// Angular and Eclipse RCP/RAP artifacts must not be added to the Maven build configuration!
			if (buildArtifact.getType() == BuildArtifactType.GUI && (project.hasAngularClient() || project.hasEclipseClient()))
				continue;

			mavenModel.getModules().add(buildArtifact.getModuleName());
		}
	}

	/**
	 * Configure the Maven model for a GUI artifact
	 */
	private void configureGUIArtifact() {
		mavenModel.setPackaging(PACKAGING_TYPE_JAR);

		if (project.hasJSFOrVaadinClient()) {
			if (project.isJakartaEEApplication())
				mavenModel.setPackaging(PACKAGING_TYPE_WAR);

			if (project.hasVaadinClient()) {
				final Dependency vaadinBoM = new Dependency();
				vaadinBoM.setArtifactId(VD_ARTIFACT_ID_BOM);
				vaadinBoM.setGroupId(VD_PLUGIN_GROUP);
				vaadinBoM.setScope(SCOPE_IMPORT);
				vaadinBoM.setType(PACKAGING_TYPE_POM);
				vaadinBoM.setVersion(VD_VERSION);

				final DependencyManagement management = new DependencyManagement();
				management.addDependency(vaadinBoM);

				mavenModel.setDependencyManagement(management);
			}
		}
	}

	/**
	 * Configure the deployment plug-in
	 * @param serverDirectory
	 */
	private void configureDeploymentPlugin(String serverDirectory) {
		final var deployPlugin = new Plugin();

		if (project.isSpringBootApplication()) {
			// An executable war file should be created for a Spring Boot application!
			deployPlugin.setArtifactId(SB_MAVEN_PLUGIN_ID);
			deployPlugin.setGroupId(SB_GROUP);

			mavenModel.getBuild().addPlugin(deployPlugin);
			return;
		}

		if (project.isDeployedOnPayara()) {
			final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);

			deployPlugin.setArtifactId(GL_PLUGIN_ID);
			deployPlugin.setVersion(GL_PLUGIN_VERSION);
			deployPlugin.setGroupId(GL_PLUGIN_GROUP);
			deployPlugin.setConfiguration(configuration);

			addElement(configuration, GL_ELEMENT_DIRECTORY, serverDirectory);
			addElement(configuration, GL_ELEMENT_USER, DEFAULT_USER);
			addElement(configuration, GL_ELEMENT_PASSWORD, DEFAULT_PASSWORD);
			addElement(configuration, GL_ELEMENT_DEBUG, GL_VALUE_DEBUG);
			addElement(configuration, GL_ELEMENT_TERSE, GL_VALUE_TERSE);
			addElement(configuration, GL_ELEMENT_ECHO, GL_VALUE_DEBUG);

			final Xpp3Dom domainElement = addElement(configuration, GL_ELEMENT_DOMAIN, null);

			addElement(domainElement, GL_ELEMENT_DOMAIN_NAME, GL_VALUE_DOMAIN);
			addElement(domainElement, GL_ELEMENT_HTTP_PORT, GL_VALUE_HTTP_PORT);
			addElement(domainElement, GL_ELEMENT_ADMIN_PORT, GL_VALUE_ADMIN_PORT);

			final Xpp3Dom componentsElement = addElement(configuration, GL_ELEMENT_COMPONENTS, null);
			final Xpp3Dom componentElement = addElement(componentsElement, GL_ELEMENT_COMPONENT, null);

			addElement(componentElement, GL_ELEMENT_ARTIFACT, GL_VALUE_COMPONENT_ARTIFACT);
			addElement(componentElement, GL_ELEMENT_COMPONENT_NAME, project.getCode());

			final var execution = new PluginExecution();
			execution.setPhase(PHASE_INSTALL);
			execution.getGoals().add(GOAL_DEPLOY);

			deployPlugin.addExecution(execution);
		}
		else if (project.isDeployedOnWildfly()) {
			final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);

			deployPlugin.setArtifactId(WILDFLY_PLUGIN_ID);
			deployPlugin.setVersion(WILDFLY_PLUGIN_VERSION);
			deployPlugin.setGroupId(WILDFLY_PLUGIN_GROUP);
			deployPlugin.setConfiguration(configuration);

			addElement(configuration, WILDFLY_ELEMENT_DIRECTORY, serverDirectory);
			addElement(configuration, WILDFLY_ELEMENT_USER, DEFAULT_USER);
			addElement(configuration, WILDFLY_ELEMENT_PASSWORD, DEFAULT_PASSWORD);
			addElement(configuration, WILDFLY_ELEMENT_NAME, project.getCode() + ".war");

			final var execution = new PluginExecution();
			execution.setPhase(PHASE_INSTALL);
			execution.getGoals().add(GOAL_DEPLOY);

			deployPlugin.addExecution(execution);
		}

		// Use a dedicated version of the Maven WAR plug-in in order to avoid build problems with newer Maven versions (e.g. 3.8)!
		final var mavenWarPlugin = new Plugin();
		mavenWarPlugin.setArtifactId(MAVEN_WAR_PLUGIN_ID);
		mavenWarPlugin.setVersion(MAVEN_WAR_PLUGIN_VERSION);

		mavenModel.getBuild().addPlugin(deployPlugin);
		mavenModel.getBuild().addPlugin(mavenWarPlugin);
	}

	/**
	 * Add all necessary plug-ins
	 * @param serverDirectory
	 */
	private void addPlugins(String serverDirectory) {
		final var build = new Build();

		mavenModel.setBuild(build);

		if (artifact.getType() == BuildArtifactType.GUI) {
			if (project.hasJSFOrVaadinClient())
				configureDeploymentPlugin(serverDirectory);

			if (project.hasVaadinClient())
				configureVaadinPlugin();
			else if (project.hasJavaFXClient() || project.hasSwingClient())
				configureAssemblyPlugin();
		}
		else if (artifact.getType() == BuildArtifactType.SERVER)
			configureDeploymentPlugin(serverDirectory);
		else if ((artifact.getType() == BuildArtifactType.CLIENT_INTERFACE || artifact.getType() == BuildArtifactType.DTO
				|| artifact.getType() == BuildArtifactType.SHARED) && project.hasEclipseClient())
			configureDependencyPlugin();
		else if (artifact.getType() == BuildArtifactType.INTEGRATION_SEI_KAFKA)
			configureAvroPlugin();
	}

	/**
	 * Configure the Vaadin plug-in for compiling the widgetset and the theme
	 */
	private void configureVaadinPlugin() {
		final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);

		final var execution = new PluginExecution();
		execution.getGoals().add(VD_GOAL_PREPARE_FRONTEND);

		final var vaadinPlugin = new Plugin();
		vaadinPlugin.setArtifactId(VD_PLUGIN_ID);
		vaadinPlugin.setVersion(VD_VERSION);
		vaadinPlugin.setGroupId(VD_PLUGIN_GROUP);
		vaadinPlugin.setConfiguration(configuration);
		vaadinPlugin.addExecution(execution);

		mavenModel.getBuild().addPlugin(vaadinPlugin);
	}

	/**
	 * Configure the Maven dependency plug-in that is necessary to copy the respective artifact to the library folder of an Eclipse
	 * RCP/RAP project
	 */
	private void configureDependencyPlugin() {
		final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);
		final var outputDir = "../../" + project.getTargetProjectName(BuildArtifactType.GUI) + "/" + LIB_FOLDER;
		final String fileName = project.getTargetProjectName(artifact.getType()) + LIB_SUFFIX;
		final Xpp3Dom artifactItems = addElement(configuration, DEP_ELEMENT_ITEMS, null);
		final Xpp3Dom artifactItem = addElement(artifactItems, DEP_ELEMENT_ITEM, null);

		addElement(artifactItem, GROUP_ID, REF_GROUP_ID);
		addElement(artifactItem, ARTIFACT_ID, REF_ARTIFACT_ID);
		addElement(artifactItem, VERSION, REF_VERSION);
		addElement(artifactItem, TYPE, PACKAGING_TYPE_JAR);
		addElement(artifactItem, DEP_ELEMENT_FILE, fileName);
		addElement(configuration, DEP_ELEMENT_DIR, outputDir);

		final var execution = new PluginExecution();
		execution.setPhase(PHASE_INSTALL);
		execution.getGoals().add(GOAL_COPY);
		execution.setConfiguration(configuration);

		final var dependencyPlugin = new Plugin();
		dependencyPlugin.setArtifactId(DEP_PLUGIN_ID);
		dependencyPlugin.setGroupId(MAVEN_PLUGIN_GROUP);
		dependencyPlugin.addExecution(execution);

		mavenModel.getBuild().addPlugin(dependencyPlugin);
	}

	/**
	 * Configure the Avro plug-in for generating Java classes based on the project's IDL files
	 */
	private void configureAvroPlugin() {
		final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);

		addElement(configuration, AVRO_ELEMENT_ST, AVRO_VALUE_ST);
		addElement(configuration, AVRO_ELEMENT_DT, AVRO_VALUE_DT);
		addElement(configuration, AVRO_ELEMENT_FV, AVRO_VALUE_FV);
		addElement(configuration, AVRO_ELEMENT_SD, AVRO_VALUE_SD);
		addElement(configuration, AVRO_ELEMENT_OD, AVRO_VALUE_OD);

		final var execution = new PluginExecution();
		execution.setPhase(PHASE_GENERATE_SOURCES);
		execution.getGoals().add(AVRO_GOAL_IDL);

		final var avroPlugin = new Plugin();
		avroPlugin.setArtifactId(AVRO_PLUGIN_ID);
		avroPlugin.setVersion(AVRO_PLUGIN_VERSION);
		avroPlugin.setGroupId(AVRO_PLUGIN_GROUP);
		avroPlugin.setConfiguration(configuration);
		avroPlugin.addExecution(execution);

		mavenModel.getBuild().addPlugin(avroPlugin);
	}

	/**
	 * Configure the Maven assembly plug-in for generating executable jar files
	 */
	private void configureAssemblyPlugin() {
		final String mainClass;
		final var configuration = new Xpp3Dom(CONFIGURATION_ELEMENT);
		final Xpp3Dom descriptorRefs = addElement(configuration, ASSEMBLY_ELEMENT_REFS, null);

		if (project.hasSwingClient())
			mainClass = project.getClientNamespace().toString() + MAIN_CLASS_SWING;
		else
			mainClass = project.getClientNamespace().toString() + "." + JAVAFX_LAUNCHER_NAME;

		addElement(descriptorRefs, ASSEMBLY_ELEMENT_REF, ASSEMBLY_VALUE_REF);

		final Xpp3Dom archive = addElement(configuration, ASSEMBLY_ELEMENT_ARCHIVE, null);
		final Xpp3Dom manifest = addElement(archive, ASSEMBLY_ELEMENT_MANIFEST, null);

		addElement(manifest, ASSEMBLY_ELEMENT_MAIN_CLASS, mainClass);

		final var execution = new PluginExecution();
		execution.setId(ASSEMBLY_EXECUTION_ID);
		execution.setPhase(PHASE_INSTALL);
		execution.getGoals().add(ASSEMBLY_GOAL);

		final var assemblyPlugin = new Plugin();
		assemblyPlugin.setArtifactId(ASSEMBLY_PLUGIN_ID);
		assemblyPlugin.setVersion(ASSEMBLY_PLUGIN_VERSION);
		assemblyPlugin.setGroupId(MAVEN_PLUGIN_GROUP);
		assemblyPlugin.setConfiguration(configuration);
		assemblyPlugin.addExecution(execution);

		mavenModel.getBuild().addPlugin(assemblyPlugin);
	}

}

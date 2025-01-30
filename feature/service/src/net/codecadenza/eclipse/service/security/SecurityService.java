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
package net.codecadenza.eclipse.service.security;

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_COMMON_MODEL_FOLDER;

import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.basic.client.imp.AngularClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.JSFClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.VaadinClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.RESTIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.RMIIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.SOAPIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.ServerProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.generator.client.imp.angular.domain.AngularDomainObjectGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.domain.AngularEnumGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.security.AngularAuthServiceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseChangePasswordDialogGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXChangePasswordDialogGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFUserSessionGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingChangePasswordDialogGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinChangePasswordViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinLoginViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinMainViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinRequestCacheGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityManagerGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinServiceInitListenerGenerator;
import net.codecadenza.eclipse.generator.integration.bean.soap.SOAPConfigGenerator;
import net.codecadenza.eclipse.generator.security.SecurityConfigurationGenerator;
import net.codecadenza.eclipse.generator.security.SecurityManagerGenerator;
import net.codecadenza.eclipse.generator.security.UserDetailsServiceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service that provides functionality regarding the security of a generated application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SecurityService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public SecurityService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild all security-related files of this project
	 * @throws Exception if an internal error has occurred
	 */
	public void rebuildAllSecurityRelatedFiles() throws Exception {
		if (project.hasAngularClient())
			rebuildAngularFiles();
		else if (project.hasJSFClient())
			rebuildJSFFiles();
		else if (project.hasVaadinClient())
			rebuildVaadinFiles();
		else if (project.hasClient())
			rebuildRichClientFiles();

		if (project.isJakartaEEApplication() || project.isSpringBootApplication()) {
			rebuildIntegrationFiles();

			// Rebuild the security configuration class
			new SecurityConfigurationGenerator(project).createSourceFile();

			if (project.isJakartaEEApplication())
				rebuildJakartaEEFiles();
			else
				rebuildSpringFiles();
		}
	}

	/**
	 * Initialize security for the given project
	 * @throws IllegalStateException if the project configuration is invalid
	 * @throws Exception if an internal error has occurred
	 */
	public void initSecurity() throws Exception {
		final var boundaryService = new BoundaryService(project);
		final var repositoryService = new RepositoryService(project);
		final var dtoService = new DTOBeanService(project);
		final DomainObject userDomainObject = project.getDomainObjectByTag(DomainTagEnumeration.USER);
		RepositoryMethod logOnRepositoryMethod = null;
		RepositoryMethod findByIdRepositoryMethod = null;

		if (project.getApplicationLogOnDTO() != null)
			throw new IllegalStateException("The log-on DTO already exists!");

		if (userDomainObject == null)
			throw new IllegalStateException("A domain object that is tagged with 'USER' could not be found!");

		// Check if all necessary tags are set properly!
		boolean activeIsSet = false;
		boolean emailIsSet = false;
		boolean nameIsSet = false;
		boolean passwordIsSet = false;

		for (final DomainAttribute attr : userDomainObject.getAttributes()) {
			if (attr.getTag() == AttributeTagEnumeration.USER_ACTIVE)
				activeIsSet = true;

			if (attr.getTag() == AttributeTagEnumeration.USER_EMAIL)
				emailIsSet = true;

			if (attr.getTag() == AttributeTagEnumeration.USER_NAME) {
				if (!attr.isDisplayAttribute()) {
					final var msg = "The log-on DTO cannot be created as the attribute that is tagged with 'USER_NAME' must be a display attribute!";

					throw new IllegalStateException(msg);
				}

				nameIsSet = true;
			}

			if (attr.getTag() == AttributeTagEnumeration.USER_PASSWORD)
				passwordIsSet = true;
		}

		if (!activeIsSet)
			throw new IllegalStateException("An attribute with the tag 'USER_ACTIVE' could not be found!");
		else if (!emailIsSet)
			throw new IllegalStateException("An attribute with the tag 'USER_EMAIL' could not be found!");
		else if (!nameIsSet)
			throw new IllegalStateException("An attribute with the tag 'USER_NAME' could not be found!");
		else if (!passwordIsSet)
			throw new IllegalStateException("An attribute with the tag 'USER_PASSWORD' could not be found!");

		// Search for the role association
		final Stream<AbstractDomainAssociation> assocStream = userDomainObject.getAssociations().stream();
		final Optional<AbstractDomainAssociation> roleAssoc = assocStream
				.filter(a -> a.getTag() == AssociationTagEnumeration.USER_ROLE).findFirst();

		if (!roleAssoc.isPresent())
			throw new IllegalStateException("The log-on DTO could not be created as no association for the roles is tagged!");

		if (project.isBoundaryMode())
			repositoryService.synchRepositories();

		boundaryService.synchBoundaries();

		// Search for necessary repository methods
		for (final Repository repository : project.getAllRepositoriesOfProject()) {
			if (!repository.getDomainObject().equals(userDomainObject))
				continue;

			for (final RepositoryMethod repositoryMethod : repository.getRepositoryMethods()) {
				if (repositoryMethod.getMethodType() == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY)
					for (final MethodParameter p : repositoryMethod.getMethodParameters()) {
						final var param = (RepositoryMethodParameter) p;

						if (param.getAttribute() != null && param.getAttribute().equals(userDomainObject.getDisplayAttribute())) {
							logOnRepositoryMethod = repositoryMethod;
							break;
						}
					}

				if (repositoryMethod.getMethodType() == RepositoryMethodTypeEnumeration.FIND_BY_ID)
					findByIdRepositoryMethod = repositoryMethod;
			}
		}

		if (logOnRepositoryMethod == null || findByIdRepositoryMethod == null)
			throw new IllegalStateException("The necessary repository methods could not be found!");

		final BoundaryBean userBoundary = boundaryService.getOrCreateBoundaryOfDomainObject(userDomainObject);
		final DTOBean roleDTO = dtoService.createRoleListDTO();
		final DTOBean logOnDTO = dtoService.createLogOnDTO(userDomainObject, roleAssoc.get(), roleDTO);

		// Create the log-on boundary method
		boundaryService.addLogOnMethodToBoundary(userBoundary, logOnRepositoryMethod, logOnDTO);

		// Create the boundary method to change the password
		boundaryService.addChangePasswordMethodToBoundary(userBoundary, findByIdRepositoryMethod, logOnDTO);

		// Synchronize integration beans
		new IntegrationBeanSyncService(project).sync();

		EclipseIDEService.saveProjectMetaData(project);
	}

	/**
	 * Rebuild all security-related files of an Angular application
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildAngularFiles() throws Exception {
		final var authServiceGenerator = new AngularAuthServiceGenerator(project);
		authServiceGenerator.createSourceFile();

		// Create all necessary services and domain objects
		for (final DTOBean dependentDTO : authServiceGenerator.getDependentDTOs()) {
			new AngularDomainObjectGenerator(dependentDTO).createSourceFile();
			new AngularServiceGenerator(project.getBoundaryByDomainObject(dependentDTO.getDomainObject())).createSourceFile();
		}

		for (final JavaEnum javaEnum : authServiceGenerator.getDependentEnums())
			new AngularEnumGenerator(javaEnum).createSourceFile();

		// Rebuild the enumeration that contains all roles
		final String content = new AngularClientProjectFilesGenerator(project).createRoleEnum();
		final var path = ANGULAR_COMMON_MODEL_FOLDER + "/role.enum.ts";

		EclipseIDEService.createOrUpdateFile(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
	}

	/**
	 * Rebuild all security-related files of a JSF application
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildJSFFiles() throws Exception {
		new JSFUserSessionGenerator(project).createSourceFile();

		final String content = JSFSecurityGenerator.createLoginPage(project);
		final var path = project.getWebAppFolder() + "/login.xhtml";

		EclipseIDEService.createOrUpdateFile(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
	}

	/**
	 * Rebuild all security-related files of a Vaadin application
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildVaadinFiles() throws Exception {
		new VaadinSecurityManagerGenerator(project).createSourceFile();
		new VaadinLoginViewGenerator(project).createSourceFile();
		new VaadinMainViewGenerator(project).createSourceFile();
		new VaadinChangePasswordViewGenerator(project).createSourceFile();

		if (project.isSpringBootApplication()) {
			new VaadinRequestCacheGenerator(project).createSourceFile();
			new VaadinServiceInitListenerGenerator(project).createSourceFile();
		}
	}

	/**
	 * Rebuild all security-related files of a rich-client application
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildRichClientFiles() throws Exception {
		new SecurityManagerGenerator(project).createSourceFile();

		if (project.hasEclipseClient())
			new EclipseChangePasswordDialogGenerator(project).createSourceFile();
		else if (project.hasSwingClient())
			new SwingChangePasswordDialogGenerator(project).createSourceFile();
		else if (project.hasJavaFXClient())
			new JavaFXChangePasswordDialogGenerator(project).createSourceFile();
	}

	/**
	 * Rebuild all security-related files of a Jakarta EE application
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildJakartaEEFiles() throws Exception {
		BuildArtifactType targetArtifact = BuildArtifactType.SERVER;
		boolean deleteDisabledHttpAuthMechanism = true;

		rebuildWebXMLFile();

		if (project.hasJSFOrVaadinClient()) {
			targetArtifact = BuildArtifactType.GUI;
			deleteDisabledHttpAuthMechanism = false;
		}

		// Rebuild the vendor-specific web application configuration file
		final String content = ServerProjectFilesGeneratorFactory.getGenerator(project).createVendorWebXML();
		final String fileName = ServerProjectFilesGeneratorFactory.getGenerator(project).getVendorWebXMLName();
		final var path = project.getWebInfFolder() + "/" + fileName;

		EclipseIDEService.createOrUpdateFile(new WorkspaceFile(project, targetArtifact, path, content));

		if (deleteDisabledHttpAuthMechanism) {
			// As soon as security for a rich-client application has been enabled the class DisabledHttpAuthenticationMechanism must be
			// deleted!
			final String packageName = project.getRootNamespace().toString();

			EclipseIDEService.deleteSource(new JavaFile(project, targetArtifact, "DisabledHttpAuthenticationMechanism", packageName));
		}
	}

	/**
	 * Rebuild all security-related files of a Spring application
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildSpringFiles() throws Exception {
		ProjectBuildFactory.getBuildService(project).rebuildSpringBootConfigurationFiles();

		// Rebuild the class UserDetailsServiceImpl
		new UserDetailsServiceGenerator(project).createSourceFile();
	}

	/**
	 * Rebuild all security-related integration files
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildIntegrationFiles() throws Exception {
		final Optional<IntegrationModule> soapModule = project.getIntegrationModules().stream()
				.filter(e -> e.getTechnology() == IntegrationTechnology.SOAP).findFirst();
		final Optional<IntegrationModule> restModule = project.getIntegrationModules().stream()
				.filter(e -> e.getTechnology() == IntegrationTechnology.REST).findFirst();
		final Optional<IntegrationModule> rmiModule = project.getIntegrationModules().stream()
				.filter(e -> e.getTechnology() == IntegrationTechnology.RMI).findFirst();

		if (soapModule.isPresent()) {
			final var soapBeanGenerator = new SOAPIntegrationProjectFilesGenerator(soapModule.get(),
					BuildArtifactType.INTEGRATION_IMP_SOAP);
			final var soapClientGenerator = new SOAPIntegrationProjectFilesGenerator(soapModule.get(),
					BuildArtifactType.INTEGRATION_CLIENT_SOAP);

			if (soapModule.get().isAddSecurityHandler()) {
				EclipseIDEService.createJavaFile(soapBeanGenerator.createAuthHandler());
				EclipseIDEService.createOrUpdateFile(soapBeanGenerator.createHandlerChain());
			}

			EclipseIDEService.createJavaFile(soapBeanGenerator.createFileServiceBean());

			if (project.isSpringBootApplication())
				new SOAPConfigGenerator(soapModule.get()).createSourceFile();

			if (soapModule.get().isAddProducers()) {
				EclipseIDEService.createJavaFile(soapClientGenerator.createCredentialsProvider());
				EclipseIDEService.createJavaFile(soapClientGenerator.createFileServiceProducer());
			}
		}

		if (restModule.isPresent()) {
			final var restBeanGenerator = new RESTIntegrationProjectFilesGenerator(restModule.get(),
					BuildArtifactType.INTEGRATION_IMP_REST);
			final var restClientGenerator = new RESTIntegrationProjectFilesGenerator(restModule.get(),
					BuildArtifactType.INTEGRATION_CLIENT_REST);

			if (restModule.get().isAddSecurityHandler())
				EclipseIDEService.createJavaFile(restBeanGenerator.createAuthFilter());

			EclipseIDEService.createJavaFile(restBeanGenerator.createRESTApplication());

			if (restModule.get().isAddProducers()) {
				EclipseIDEService.createJavaFile(restClientGenerator.createCredentialsProvider());
				EclipseIDEService.createJavaFile(restClientGenerator.createFileServiceProducer());
			}
		}

		if (project.isJakartaEEApplication() && rmiModule.isPresent() && rmiModule.get().isAddProducers()) {
			final var rmiClientGenerator = new RMIIntegrationProjectFilesGenerator(rmiModule.get(),
					BuildArtifactType.INTEGRATION_CLIENT_RMI);

			EclipseIDEService.createJavaFile(rmiClientGenerator.createCredentialsProvider());
			EclipseIDEService.createJavaFile(rmiClientGenerator.createFileServiceProducer());
		}
	}

	/**
	 * Rebuild the web.xml file
	 * @throws Exception if an internal error has occurred
	 */
	private void rebuildWebXMLFile() throws Exception {
		final var path = project.getWebInfFolder() + "/web.xml";
		final BuildArtifactType targetArtifact;
		final String content;

		if (project.hasJSFClient()) {
			targetArtifact = BuildArtifactType.GUI;
			content = new JSFClientProjectFilesGenerator(project).createWebXMLFile();
		}
		else if (project.hasVaadinClient()) {
			targetArtifact = BuildArtifactType.GUI;
			content = new VaadinClientProjectFilesGenerator(project).createWebXML();
		}
		else {
			targetArtifact = BuildArtifactType.SERVER;
			content = ServerProjectFilesGeneratorFactory.getGenerator(project).createWebXML();
		}

		EclipseIDEService.createOrUpdateFile(new WorkspaceFile(project, targetArtifact, path, content));
	}

}

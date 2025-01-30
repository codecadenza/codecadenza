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
package net.codecadenza.eclipse.service.integration;

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.basic.integration.imp.JMSIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.RESTIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.integration.bean.common.IntegrationServiceProducerGenerator;
import net.codecadenza.eclipse.generator.integration.bean.jms.JMSIntegrationBeanGenerator;
import net.codecadenza.eclipse.generator.integration.bean.jms.JMSIntegrationClientGenerator;
import net.codecadenza.eclipse.generator.integration.bean.jms.JMSIntegrationSEIGenerator;
import net.codecadenza.eclipse.generator.integration.bean.kafka.KafkaIntegrationBeanGenerator;
import net.codecadenza.eclipse.generator.integration.bean.kafka.KafkaIntegrationClientGenerator;
import net.codecadenza.eclipse.generator.integration.bean.kafka.KafkaIntegrationIDLGenerator;
import net.codecadenza.eclipse.generator.integration.bean.kafka.KafkaIntegrationSEIGenerator;
import net.codecadenza.eclipse.generator.integration.bean.rest.RESTIntegrationBeanGenerator;
import net.codecadenza.eclipse.generator.integration.bean.rest.RESTIntegrationClientGenerator;
import net.codecadenza.eclipse.generator.integration.bean.rest.RESTIntegrationSEIGenerator;
import net.codecadenza.eclipse.generator.integration.bean.rmi.RMIIntegrationBeanGenerator;
import net.codecadenza.eclipse.generator.integration.bean.rmi.RMIIntegrationClientGenerator;
import net.codecadenza.eclipse.generator.integration.bean.rmi.RMIIntegrationSEIGenerator;
import net.codecadenza.eclipse.generator.integration.bean.soap.SOAPConfigGenerator;
import net.codecadenza.eclipse.generator.integration.bean.soap.SOAPIntegrationBeanGenerator;
import net.codecadenza.eclipse.generator.integration.bean.soap.SOAPIntegrationClientGenerator;
import net.codecadenza.eclipse.generator.integration.bean.soap.SOAPIntegrationSEIGenerator;
import net.codecadenza.eclipse.generator.integration.method.IntegrationMethodGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.service.CodeCadenzaServicePlugin;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.util.maven.MavenBuildTool;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Utility class that provides basic services for integration beans
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationBeanService {
	private final Project project;
	private final DTOBeanService dtoService;

	/**
	 * Constructor
	 * @param project
	 */
	public IntegrationBeanService(Project project) {
		this.project = project;
		this.dtoService = new DTOBeanService(project);
	}

	/**
	 * Rebuild the integration bean source files
	 * @param integrationBean
	 * @throws Exception if the rebuild operation has failed
	 */
	public void rebuildIntegrationBeanSourceFiles(AbstractIntegrationBean integrationBean) throws Exception {
		final String interfaceName = integrationBean.getInterfaceName();
		final String clientName = integrationBean.getClientClassName();
		final IntegrationModule module = integrationBean.getIntegrationModule();

		if (module.hasSEIArtifact() && interfaceName != null && !interfaceName.isEmpty()) {
			// Create the service interface
			if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.SOAP)
				new SOAPIntegrationSEIGenerator(integrationBean).createSourceFile();
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.REST)
				new RESTIntegrationSEIGenerator(integrationBean).createSourceFile();
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.RMI)
				new RMIIntegrationSEIGenerator(integrationBean).createSourceFile();
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.JMS)
				new JMSIntegrationSEIGenerator(integrationBean).createSourceFile();
		}

		if (module.hasClientArtifact() && clientName != null && !clientName.isEmpty()) {
			// Create the client
			if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.SOAP)
				new SOAPIntegrationClientGenerator(integrationBean).createSourceFile();
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.REST)
				new RESTIntegrationClientGenerator(integrationBean).createSourceFile();
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.RMI)
				new RMIIntegrationClientGenerator(integrationBean).createSourceFile();
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA) {
				new KafkaIntegrationSEIGenerator(integrationBean).createSourceFile();
				new KafkaIntegrationClientGenerator(integrationBean).createSourceFile();
			}
			else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.JMS)
				new JMSIntegrationClientGenerator(integrationBean).createSourceFile();

			// Create the producer
			if (module.isAddProducers() && integrationBean.getProducerClassName() != null
					&& !integrationBean.getProducerClassName().isEmpty())
				new IntegrationServiceProducerGenerator(integrationBean).createSourceFile();
		}

		// Create the service bean
		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.SOAP)
			new SOAPIntegrationBeanGenerator(integrationBean).createSourceFile();
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.REST)
			new RESTIntegrationBeanGenerator(integrationBean).createSourceFile();
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.RMI)
			new RMIIntegrationBeanGenerator(integrationBean).createSourceFile();
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA) {
			new KafkaIntegrationIDLGenerator(integrationBean).createSourceFile();
			new KafkaIntegrationBeanGenerator(integrationBean).createSourceFile();
		}
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.JMS)
			new JMSIntegrationBeanGenerator(integrationBean).createSourceFile();

		rebuildConfigurationFiles(module);

		// Rebuild all data transfer objects
		rebuildDTOsOfIntegrationBean(integrationBean);
	}

	/**
	 * Remove the given integration bean
	 * @param integrationBean
	 * @throws Exception if either the integration bean or the respective source file could not be deleted
	 */
	public void removeIntegrationBean(AbstractIntegrationBean integrationBean) throws Exception {
		final Namespace namespace = integrationBean.getNamespace();
		final IntegrationModule module = integrationBean.getIntegrationModule();

		removeIntegrationBeanSourceFiles(integrationBean);

		namespace.eResource().getContents().remove(integrationBean);

		// Search for unused virtual facade methods and remove them
		new BoundaryService(project).removeUnusedVirtualMethods();

		// Search for unused virtual data transfer objects and remove them
		dtoService.removeUnusedVirtualDTOs();

		EclipseIDEService.saveProjectMetaData(project);

		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			generateJavaClassesFromAvroIDLFiles();

		// Rebuild all data transfer objects
		rebuildDTOsOfIntegrationBean(integrationBean);

		// The bean must be removed from the list of all Java types. Otherwise, it will be added again to the respective configuration
		// file!
		namespace.getJavaTypes().remove(integrationBean);

		rebuildConfigurationFiles(module);
	}

	/**
	 * Remove the given integration method
	 * @param method
	 * @throws Exception if an unexpected error has occurred
	 */
	public void removeIntegrationMethod(AbstractIntegrationMethod method) throws Exception {
		final AbstractIntegrationBean integrationBean = method.getIntegrationBean();

		integrationBean.getMethods().remove(method);

		// Search for unused virtual facade methods and remove them
		new BoundaryService(project).removeUnusedVirtualMethods();

		// Search for unused virtual data transfer objects and delete them
		dtoService.removeUnusedVirtualDTOs();

		// Save changes and rebuild the integration source files
		EclipseIDEService.saveProjectMetaData(project);

		rebuildIntegrationBeanSourceFiles(integrationBean);

		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			generateJavaClassesFromAvroIDLFiles();
	}

	/**
	 * Remove the integration bean source files
	 * @param integrationBean
	 * @throws Exception if the integration bean source files could not be deleted
	 */
	public void removeIntegrationBeanSourceFiles(AbstractIntegrationBean integrationBean) throws Exception {
		final String seiName = integrationBean.getInterfaceName();
		final String clientName = integrationBean.getClientClassName();
		final IntegrationModule module = integrationBean.getIntegrationModule();
		final JavaFile serviceBean = integrationBean.getServiceBeanSourceFile();

		if (module.hasClientArtifact() && clientName != null && !clientName.isEmpty()) {
			// Remove the client
			EclipseIDEService.deleteSource(integrationBean.getClientSourceFile());

			// Remove the producer
			if (module.isAddProducers() && integrationBean.getProducerClassName() != null
					&& !integrationBean.getProducerClassName().isEmpty())
				EclipseIDEService.deleteSource(integrationBean.getProducerSourceFile());
		}

		// Remove the service end-point interface
		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA) {
			final var fileName = integrationBean.getDomainObject().getName().toLowerCase() + ".avdl";
			final var path = project.getResourceFolder() + "/" + fileName;
			final String projectName = project.getTargetProjectName(BuildArtifactType.INTEGRATION_SEI_KAFKA);

			EclipseIDEService.deleteWorkspaceFile(projectName, path);

			if (module.hasClientArtifact())
				EclipseIDEService.deleteSource(integrationBean.getSEISourceFile());
		}
		else if (module.hasSEIArtifact() && seiName != null && !seiName.isEmpty())
			EclipseIDEService.deleteSource(integrationBean.getSEISourceFile());

		// Remove the service implementation
		EclipseIDEService.deleteSource(serviceBean);
	}

	/**
	 * @param integrationBean
	 * @return a set containing all boundaries that are used by the given integration bean
	 */
	public Set<BoundaryBean> getBoundariesOfIntegrationBean(AbstractIntegrationBean integrationBean) {
		final var boundaries = new HashSet<BoundaryBean>();

		integrationBean.getMethods().forEach(method -> {
			final Map<String, ServiceBean> serviceMap = IntegrationMethodGeneratorFactory.getMethodGenerator(method, null)
					.getServices();

			boundaries.addAll(serviceMap.values().stream().filter(BoundaryBean.class::isInstance).map(BoundaryBean.class::cast)
					.collect(Collectors.toSet()));
		});

		return boundaries;
	}

	/**
	 * @param integrationMethod
	 * @return the signature of the given integration method
	 */
	public String getMethodSignature(AbstractIntegrationMethod integrationMethod) {
		return IntegrationMethodGeneratorFactory.getMethodGenerator(integrationMethod, null).getMethodSignature(false, false, false);
	}

	/**
	 * Check if the provided names are unique within an integration module
	 * @param module
	 * @param integrationBean
	 * @param beanName
	 * @param interfaceName
	 * @param clientName
	 * @param producerName
	 * @return a message if the validation has failed
	 */
	public String checkUniqueClassNames(IntegrationModule module, AbstractIntegrationBean integrationBean, String beanName,
			String interfaceName, String clientName, String producerName) {
		boolean valid = true;

		if (beanName.equals(interfaceName) || beanName.equals(clientName) || beanName.equals(producerName))
			valid = false;

		if (interfaceName != null && !interfaceName.isEmpty()
				&& (interfaceName.equals(clientName) || interfaceName.equals(producerName)))
			valid = false;

		if (clientName != null && !clientName.isEmpty() && clientName.equals(producerName))
			valid = false;

		if (!valid)
			return "The class names of this integration bean must be unique!";

		final Optional<AbstractIntegrationBean> existingIntegrationBean = module.getNamespace().getJavaTypes().stream()
				.map(AbstractIntegrationBean.class::cast).filter(bean -> !bean.equals(integrationBean))
				.filter(bean -> checkUniqueClassNames(bean, beanName, interfaceName, clientName, producerName)).findFirst();

		if (existingIntegrationBean.isPresent()) {
			final var message = new StringBuilder();
			message.append("All integration class names must be unique within an integration ");
			message.append("module! At least one class name of integration bean '");
			message.append(existingIntegrationBean.get().getName());
			message.append("' is equal to a class name of this integration bean!");

			return message.toString();
		}

		return null;
	}

	/**
	 * Check if the given Avro schema name is already used in another method
	 * @param integrationMethod the method that should not be considered
	 * @param schemaName the schema name that should be checked
	 * @return false if the validation has failed
	 */
	public boolean validateSchemaName(AbstractIntegrationMethod integrationMethod, String schemaName) {
		if (schemaName.isEmpty())
			return true;

		return project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_KAFKA).getNamespace().getJavaTypes().stream()
				.map(KafkaIntegrationBean.class::cast).map(AbstractIntegrationBean::getMethods).flatMap(Collection::stream)
				.filter(method -> !method.equals(integrationMethod)).map(KafkaIntegrationMethod.class::cast).noneMatch(
						method -> method.getResponseSchemaName().equals(schemaName) || method.getRequestSchemaName().equals(schemaName));
	}

	/**
	 * Check if the given destination name is already used in another bean
	 * @param integrationBean the integration bean that should not be considered
	 * @param destinationName the name of the destination that should be checked
	 * @return false if the validation has failed
	 */
	public boolean validateDestinationName(AbstractIntegrationBean integrationBean, String destinationName) {
		final boolean valid = project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_JMS).getNamespace()
				.getJavaTypes().stream().map(JMSIntegrationBean.class::cast).filter(bean -> !bean.equals(integrationBean))
				.map(JMSIntegrationBean::getRequestDestination).map(JMSResource::getName).noneMatch(name -> name.equals(destinationName));

		if (!valid)
			return false;

		return project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_JMS).getNamespace().getJavaTypes().stream()
				.map(JMSIntegrationBean.class::cast).filter(bean -> !bean.equals(integrationBean))
				.map(JMSIntegrationBean::getResponseDestination).map(JMSResource::getName)
				.noneMatch(name -> name.equals(destinationName));
	}

	/**
	 * Generate all Java classes from the project's Avro IDL files by running a respective Maven goal
	 * @throws Exception if an internal error has occurred
	 */
	public void generateJavaClassesFromAvroIDLFiles() throws Exception {
		final IntegrationModule module = project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_SEI_KAFKA);
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject wsProject = wsRoot.getProject(project.getTargetProjectName(BuildArtifactType.INTEGRATION_SEI_KAFKA));
		final IJavaProject javaProject = JavaCore.create(wsProject);
		final IFolder folder = wsProject.getFolder(project.getSourceFolder());
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(module.getNamespace().toString() + SUB_PACKAGE_INT_AVRO);

		// Delete all existing Avro classes
		for (final var javaElement : fragment.getChildren()) {
			final ICompilationUnit compUnit = fragment.getCompilationUnit(javaElement.getElementName());

			if (compUnit.exists())
				compUnit.delete(true, null);
		}

		final Job job = Job.create("Generate Avro classes", monitor -> {
			try {
				// Run the Maven build
				MavenBuildTool.generateSources(wsProject, monitor);

				// Refresh the project
				wsProject.refreshLocal(IResource.DEPTH_INFINITE, monitor);

				return Status.OK_STATUS;
			}
			catch (final Exception e) {
				return new Status(IStatus.ERROR, CodeCadenzaServicePlugin.PLUGIN_ID, e.getMessage(), e);
			}
		});

		job.setUser(true);
		job.schedule();
	}

	/**
	 * Check if the class names of an integration bean are unique compared to the provided names
	 * @param integrationBean
	 * @param beanName
	 * @param interfaceName
	 * @param clientName
	 * @param producerName
	 * @return true if at least one of the given class names is not unique compared to the class names of the this integration bean
	 */
	private boolean checkUniqueClassNames(AbstractIntegrationBean integrationBean, String beanName, String interfaceName,
			String clientName, String producerName) {
		final String thisBeanName = integrationBean.getName();
		final String thisInterfaceName = integrationBean.getInterfaceName();
		final String thisClientName = integrationBean.getClientClassName();
		final String thisProducerName = integrationBean.getProducerClassName();

		if (thisBeanName.equals(beanName) || thisBeanName.equals(interfaceName) || thisBeanName.equals(clientName)
				|| thisBeanName.equals(producerName))
			return true;

		if (thisInterfaceName != null && !thisInterfaceName.isEmpty()
				&& (thisInterfaceName.equals(beanName) || thisInterfaceName.equals(interfaceName) || thisInterfaceName.equals(clientName)
						|| thisInterfaceName.equals(producerName)))
			return true;

		if (thisClientName != null && !thisClientName.isEmpty() && (thisClientName.equals(beanName)
				|| thisClientName.equals(interfaceName) || thisClientName.equals(clientName) || thisClientName.equals(producerName)))
			return true;

		return thisProducerName != null && !thisProducerName.isEmpty()
				&& (thisProducerName.equals(beanName) || thisProducerName.equals(interfaceName) || thisProducerName.equals(clientName)
						|| thisProducerName.equals(producerName));
	}

	/**
	 * Rebuild all data transfer objects that are used by this integration bean
	 * @param integrationBean
	 * @throws Exception if rebuilding of the data transfer objects has failed
	 */
	private void rebuildDTOsOfIntegrationBean(AbstractIntegrationBean integrationBean) throws Exception {
		final var dtoBeans = new ArrayList<DTOBean>();

		integrationBean.getMethods().forEach(method -> {
			if (method.getReturnType() instanceof final DTOBean dto) {
				for (final DTOBeanAttribute attr : dto.getAttributes())
					if (attr.getReferencedDTOBean() != null)
						dtoBeans.add(attr.getReferencedDTOBean());

				dtoBeans.add(dto);
			}

			for (final MethodParameter param : method.getMethodParameters())
				if (param.getType() instanceof final DTOBean dto) {
					for (final DTOBeanAttribute attr : dto.getAttributes())
						if (attr.getReferencedDTOBean() != null)
							dtoBeans.add(attr.getReferencedDTOBean());

					dtoBeans.add(dto);
				}
		});

		for (final DTOBean dto : dtoBeans)
			dtoService.rebuildDTOBeanSourceFiles(dto);
	}

	/**
	 * Rebuild the configuration files of the given integration module
	 * @param module
	 * @throws Exception if the rebuild operation has failed
	 */
	private void rebuildConfigurationFiles(IntegrationModule module) throws Exception {
		// In case of a Jakarta EE application, no file has to be rebuilt!
		if (project.isJakartaEEApplication())
			return;

		if (module.getTechnology() == IntegrationTechnology.REST) {
			final var restGenerator = new RESTIntegrationProjectFilesGenerator(module, BuildArtifactType.INTEGRATION_IMP_REST);

			EclipseIDEService.createJavaFile(restGenerator.createRESTApplication());
		}
		else if (module.getTechnology() == IntegrationTechnology.SOAP)
			new SOAPConfigGenerator(module).createSourceFile();
		else if (module.getTechnology() == IntegrationTechnology.JMS && module.hasClientArtifact())
			new JMSIntegrationProjectFilesGenerator(module, BuildArtifactType.INTEGRATION_CLIENT_JMS).rebuildJNDIProperties();
	}

}

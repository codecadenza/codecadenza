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
package net.codecadenza.eclipse.service.exchange;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_MAPPING_OBJ_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_PARAM_IMP_STRING;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_ROOT_MAPPING_OBJ_NAME;
import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_SUFFIX;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.generator.exchange.DataExchangeBeanGenerator;
import net.codecadenza.eclipse.generator.exchange.add.XMLSchemaGenerator;
import net.codecadenza.eclipse.generator.exchange.method.imp.DataExchangeMethodGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.ScheduledInvocation;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.mapping.MappingObjectService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;

/**
 * <p>
 * Service for data exchange service beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataExchangeBeanService {
	private static final String DEFAULT_FILTER_PARAM_OPERATOR = "=";

	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public DataExchangeBeanService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild the source files
	 * @param exchangeBean
	 * @throws Exception if the data exchange source files could not be rebuilt
	 */
	public void rebuildDataExchangeServiceBeanSourceFiles(DataExchangeServiceBean exchangeBean) throws Exception {
		// Create the exchange bean
		new DataExchangeBeanGenerator(exchangeBean).createSourceFile();

		for (final DataExchangeMethod method : exchangeBean.getDataExchangeMethods())
			rebuildSchemaFile(method);
	}

	/**
	 * Build the schema file for the given exchange method
	 * @param exchangeMethod
	 * @throws Exception if the XML schema file could not be rebuilt
	 */
	public void rebuildSchemaFile(DataExchangeMethod exchangeMethod) throws Exception {
		if (exchangeMethod.getContentType() != ContentTypeEnumeration.XML)
			return;

		if (exchangeMethod.getSchemaFileName() == null || exchangeMethod.getSchemaFileName().isEmpty())
			return;

		final String content = XMLSchemaGenerator.createXMLSchema(exchangeMethod.getRootElement(true));
		final var path = project.getSchemaFolder() + "/" + exchangeMethod.getSchemaFileName();
		final WorkspaceFile schemaFile;

		if (project.isJakartaEEApplication()) {
			if (project.hasJSFOrVaadinClient())
				schemaFile = new WorkspaceFile(project, BuildArtifactType.GUI, path, content);
			else
				schemaFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);
		}
		else
			schemaFile = new WorkspaceFile(project, BuildArtifactType.DATA_EXCHANGE, path, content);

		EclipseIDEService.createOrUpdateFile(schemaFile);
	}

	/**
	 * Rebuild all files that belong to this data exchange method
	 * @param method
	 * @throws Exception if one of the files could not be rebuilt
	 */
	public void rebuildAllFilesOfMethod(DataExchangeMethod method) throws Exception {
		final var exchangeMappingObjectService = new ExchangeMappingObjectService();

		for (final DataExchangeElement element : method.getRootElement(true).getAllElements())
			if (element.getMappingObject() != null)
				exchangeMappingObjectService.rebuildExchangeMappingObject(element.getMappingObject());

		rebuildDataExchangeServiceBeanSourceFiles(method.getDataExchangeServiceBean());
	}

	/**
	 * Delete the given data exchange bean
	 * @param exchangeBean
	 * @throws Exception if the data exchange bean could not be deleted
	 */
	public void deleteDataExchangeServiceBean(DataExchangeServiceBean exchangeBean) throws Exception {
		final Namespace ns = exchangeBean.getNamespace();

		for (final DataExchangeMethod method : exchangeBean.getDataExchangeMethods())
			deleteDataExchangeMethod(method, true);

		// Delete the source file
		EclipseIDEService.deleteSource(exchangeBean.getSourceFile());

		ns.getJavaTypes().remove(exchangeBean);
		project.eResource().getContents().remove(exchangeBean);

		EclipseIDEService.saveProjectMetaData(project);
	}

	/**
	 * Delete the given data exchange method
	 * @param method
	 * @param callerDeletesDataExchangeServiceBean a flag that indicates whether respective data exchange service bean will be
	 *          deleted by calling method
	 * @throws IllegalStateException if the method is referenced by other objects of the internal meta-model
	 * @throws Exception if a respective file could not be deleted
	 */
	public void deleteDataExchangeMethod(DataExchangeMethod method, boolean callerDeletesDataExchangeServiceBean) throws Exception {
		final DataExchangeServiceBean exchangeBean = method.getDataExchangeServiceBean();
		String schemaFileName = null;

		// Check if a boundary method exists that references this method!
		for (final BoundaryBean boundary : project.getAllBoundariesOfProject())
			for (final BoundaryMethod boundaryMethod : boundary.getBoundaryMethods()) {
				if (boundaryMethod.getServiceMethod() == null)
					continue;

				if (boundaryMethod.getServiceMethod().equals(method)) {
					var msg = "The data exchange method '" + method.getName() + "' ";
					msg += "cannot be deleted as it is referenced by ";

					if (project.isBoundaryMode())
						msg += "boundary";
					else
						msg += "facade";

					msg += " method '" + boundaryMethod.getName() + "'!";

					throw new IllegalStateException(msg);
				}
			}

		// If the method has no own root element the mapping objects shouldn't be deleted!
		if (method.getRootElement() != null) {
			// Check if another method is joined with this method! This check can be skipped if the calling method deletes the data
			// exchange service bean anyway!
			if (method.getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT && !callerDeletesDataExchangeServiceBean)
				for (final DataExchangeMethod m : exchangeBean.getDataExchangeMethods())
					if (m.getJoinedImportMethod() != null && m.getJoinedImportMethod().equals(method)) {
						final var msg = "The data exchange method '" + method.getName() + "' cannot be deleted as method '" + m.getName()
								+ "' is joined with it!";

						throw new IllegalStateException(msg);
					}

			if (method.getContentType() == ContentTypeEnumeration.XML)
				schemaFileName = method.getSchemaFileName();

			for (final DataExchangeElement element : method.getRootElement().getAllElements())
				if (element.getMappingObject() != null) {
					final ExchangeMappingObject mappingObject = element.getMappingObject();
					final Namespace ns = mappingObject.getNamespace();

					if (ns != null) {
						EclipseIDEService.deleteSource(mappingObject.getSourceFile());

						ns.getJavaTypes().remove(mappingObject);
						project.eResource().getContents().remove(mappingObject);
					}
				}
		}

		if (schemaFileName != null && !schemaFileName.isEmpty()) {
			final var schemaFilePath = project.getSchemaFolder() + "/" + schemaFileName;
			final String projectName;

			if (project.isJakartaEEApplication()) {
				if (project.hasJSFOrVaadinClient())
					projectName = project.getTargetProjectName(BuildArtifactType.GUI);
				else
					projectName = project.getTargetProjectName(BuildArtifactType.SERVER);
			}
			else
				projectName = project.getTargetProjectName(BuildArtifactType.DATA_EXCHANGE);

			EclipseIDEService.deleteWorkspaceFile(projectName, schemaFilePath);
		}

		if (!callerDeletesDataExchangeServiceBean) {
			exchangeBean.getDataExchangeMethods().remove(method);
			project.eResource().getContents().remove(method);

			rebuildDataExchangeServiceBeanSourceFiles(exchangeBean);

			EclipseIDEService.saveProjectMetaData(project);
		}
	}

	/**
	 * Validate the invocation settings
	 * @param method
	 * @throws IllegalStateException if the invocation settings are invalid!
	 */
	public void checkValidInvocationSettings(DataExchangeMethod method) {
		if (!project.isJakartaEEApplication() || method.getMethodInvocation() == null)
			return;

		if (method.getMethodInvocation() instanceof AsynchronousInvocation && !method.getReturnType().isVoid())
			throw new IllegalStateException("An asynchronous method must return void!");

		if (method.getMethodInvocation() instanceof ScheduledInvocation) {
			if (!method.getReturnType().isVoid())
				throw new IllegalStateException("A scheduled method must return void!");

			if (!method.getMethodParameters().isEmpty())
				throw new IllegalStateException("A scheduled method must not have a parameter!");
		}
	}

	/**
	 * Validate the method data
	 * @param method
	 * @param exchangeBean
	 * @throws IllegalStateException if the validation has failed
	 */
	public void validateMethodData(DataExchangeMethod method, DataExchangeServiceBean exchangeBean) {
		final var paramNameSet = new HashSet<String>();
		final String methodSignature = generateMethodSignature(method);

		for (final DataExchangeMethod m : exchangeBean.getDataExchangeMethods())
			if (!m.equals(method) && generateMethodSignature(m).equals(methodSignature))
				throw new IllegalStateException("The data exchange service bean already contains a method with the same signature!");

		// Check the filter parameter names
		for (final MethodParameter param : method.getMethodParameters())
			if (param instanceof FilterMethodParameter) {
				if (paramNameSet.contains(param.getName()))
					throw new IllegalStateException("A filter parameter with the name '" + param.getName() + "' already exists!");

				paramNameSet.add(param.getName());
			}

		checkValidInvocationSettings(method);

		// Check the exchange structure regarding valid names!
		if (method.getRootElement() == null)
			return;

		// Check if the XML schema file name is unique
		checkSchemaFileName(method);

		method.getRootElement().checkElementStructure();

		// Check if a mapping object with the same name already exists in this package!
		for (final DataExchangeElement element : method.getRootElement().getAllElements()) {
			if (element.getMappingObject() == null)
				continue;

			final ExchangeMappingObject mappingObject = element.getMappingObject();
			final IStatus status = new MappingObjectService(project).validateMappingObject(mappingObject);

			if (!status.isOK())
				throw new IllegalStateException(status.getMessage());

			final var nameSet = new HashSet<String>();

			// Check the mapping attribute names
			for (final ExchangeMappingAttribute attr : mappingObject.getAttributes()) {
				if (nameSet.contains(attr.getName()))
					throw new IllegalStateException("A mapping attribute with the name '" + attr.getName() + "' already exists!");

				nameSet.add(attr.getName());
			}
		}
	}

	/**
	 * Search a data exchange service bean by the given domain object
	 * @param domainObject
	 * @return the data exchange service bean if it exists
	 */
	public DataExchangeServiceBean getDataExchangeServiceByDomainObject(DomainObject domainObject) {
		final Namespace exchangeNamespace = project.getExchangeNamespace().getChildNamespaces().stream()
				.filter(ns -> ns.getName().equals(domainObject.getNamespace().getName())).findFirst().orElse(null);

		if (exchangeNamespace == null)
			return null;

		for (final JavaType type : exchangeNamespace.getJavaTypes()) {
			if (!(type instanceof final DataExchangeServiceBean exchangeBean))
				continue;

			if (exchangeBean.getDomainObject().equals(domainObject))
				return exchangeBean;
		}

		return null;
	}

	/**
	 * Create new data exchange service bean
	 * @param domainObject
	 * @return the new data exchange service bean
	 */
	public DataExchangeServiceBean createNewDataExchangeServiceBean(DomainObject domainObject) {
		final DataExchangeServiceBean exchangeBean = ExchangeFactory.eINSTANCE.createDataExchangeServiceBean();
		exchangeBean.setDomainObject(domainObject);
		exchangeBean.setPrimitive(false);
		exchangeBean.setMappable(false);
		exchangeBean.setComment("Data exchange service for " + domainObject.getLabel() + " objects");
		exchangeBean.setName(domainObject.getName() + EXCHANGE_SUFFIX);

		for (final Namespace ns : project.getExchangeNamespace().getChildNamespaces())
			if (ns.getName().equals(domainObject.getNamespace().getName())) {
				exchangeBean.setNamespace(ns);
				ns.getJavaTypes().add(exchangeBean);
				break;
			}

		return exchangeBean;
	}

	/**
	 * Save changes to the existing data exchange method
	 * @param dataExchangeMethod
	 * @param addToBoundary
	 * @throws Exception if the update operation has failed
	 */
	public void updateDataExchangeMethod(final DataExchangeMethod dataExchangeMethod, final boolean addToBoundary)
			throws Exception {
		validateMethodData(dataExchangeMethod);

		saveDataExchangeMethod(dataExchangeMethod, addToBoundary);
	}

	/**
	 * Create a new data exchange method
	 * @param method
	 * @param domainObject
	 * @param addToBoundary
	 * @throws Exception if the create operation has failed
	 */
	public void createNewDataExchangeMethod(final DataExchangeMethod method, final DomainObject domainObject,
			final boolean addToBoundary) throws Exception {
		DataExchangeServiceBean exchangeBean = getDataExchangeServiceByDomainObject(domainObject);

		if (exchangeBean == null)
			exchangeBean = createNewDataExchangeServiceBean(domainObject);

		validateMethodData(method, exchangeBean);

		method.setDataExchangeServiceBean(exchangeBean);
		exchangeBean.getDataExchangeMethods().add(method);

		if (method.getContentType() == ContentTypeEnumeration.XML)
			method.setParser(ParserImplementationEnumeration.JAXB);
		else if (method.getContentType() == ContentTypeEnumeration.CSV)
			method.setParser(ParserImplementationEnumeration.APACHE_COMMONS);
		else if (method.getContentType() == ContentTypeEnumeration.JSON)
			method.setParser(ParserImplementationEnumeration.JSONB);
		else
			method.setParser(ParserImplementationEnumeration.POI);

		if (method.getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT) {
			if (method.getExchangeMode() instanceof DirectExchangeMode)
				method.setReturnType(method.getRootElement(true).getMappingObject());
			else if (method.getExchangeMode() instanceof StringExchangeMode)
				method.setReturnType(project.getJavaTypeByName(JavaType.STRING));

			if (method.isProcessSingleObject()) {
				final FilterMethodParameter param = ExchangeFactory.eINSTANCE.createFilterMethodParameter();
				param.setDomainAttribute(exchangeBean.getDomainObject().getPKAttribute());
				param.setType(exchangeBean.getDomainObject().getPKAttribute().getJavaType());
				param.setName(exchangeBean.getDomainObject().getPKAttribute().getName());
				param.setOperator(DEFAULT_FILTER_PARAM_OPERATOR);
				param.setMethod(method);

				method.getMethodParameters().add(param);
			}
		}
		else if (method.getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT) {
			if (method.getExchangeMode() instanceof StringExchangeMode) {
				final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
				param.setName(DEFAULT_PARAM_IMP_STRING);
				param.setType(project.getJavaTypeByName(JavaType.STRING));
				param.setMethod(method);

				method.getMethodParameters().add(param);
			}
			else if (method.getExchangeMode() instanceof DirectExchangeMode) {
				final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();

				if (method.isProcessSingleObject())
					param.setName(DEFAULT_MAPPING_OBJ_NAME);
				else
					param.setName(DEFAULT_ROOT_MAPPING_OBJ_NAME);

				param.setType(method.getRootElement(true).getMappingObject());
				param.setMethod(method);

				method.getMethodParameters().add(param);
			}
		}

		saveDataExchangeMethod(method, addToBoundary);
	}

	/**
	 * @param exchangeBean
	 * @return a set containing all repositories necessary for a given data exchange service bean
	 */
	public Set<Repository> getAllRepositoriesOfExchangeService(DataExchangeServiceBean exchangeBean) {
		final var repositories = new HashSet<Repository>();

		exchangeBean.getDataExchangeMethods().forEach(method -> {
			final Map<String, Repository> repoMap = DataExchangeMethodGeneratorFactory.getMethodGenerator(null, method)
					.getRepositories();
			repositories.addAll(repoMap.values());
		});

		return repositories;
	}

	/**
	 * @param exchangeMethod
	 * @return the signature of the given data exchange method
	 */
	public String getMethodSignature(DataExchangeMethod exchangeMethod) {
		return DataExchangeMethodGeneratorFactory.getMethodGenerator(null, exchangeMethod).getMethodSignature(true);
	}

	/**
	 * Save the data exchange method
	 * @param method
	 * @param addToBoundary
	 * @throws Exception if the save operation has failed
	 */
	private void saveDataExchangeMethod(final DataExchangeMethod method, final boolean addToBoundary) throws Exception {
		final DataExchangeServiceBean exchangeBean = method.getDataExchangeServiceBean();
		final var boundaryService = new BoundaryService(project);
		final var repositoryService = new RepositoryService(project);
		BoundaryMethod boundaryMethod = null;
		Set<Repository> repositories = new HashSet<>();
		Set<BoundaryBean> boundarySet = new HashSet<>();

		// Add all mapping objects to the meta-model
		if (method.getRootElement() != null) {
			for (final DataExchangeElement element : method.getRootElement().getAllElements()) {
				final ExchangeMappingObject mappingObject = element.getMappingObject();

				if (mappingObject == null)
					continue;

				addMappingObjectToNamespace(mappingObject);

				if (!project.eResource().getContents().contains(mappingObject))
					project.eResource().getContents().add(mappingObject);
			}
		}

		// Create or update objects this service depends on!
		if (project.isBoundaryMode())
			repositories = repositoryService.synchRepositories();
		else
			boundarySet = boundaryService.synchBoundaries();

		if (addToBoundary)
			boundaryMethod = boundaryService.initializeBoundaryMethod(method);

		if (!project.eResource().getContents().contains(exchangeBean))
			project.eResource().getContents().add(exchangeBean);

		if (boundaryMethod != null) {
			final BoundaryBean boundary = boundaryMethod.getBoundaryBean();

			boundarySet.add(boundary);

			// If necessary, add the boundary to the meta-model
			if (!project.eResource().getContents().contains(boundary))
				project.eResource().getContents().add(boundary);

			// If necessary, add the repository to the meta-model
			if (!project.eResource().getContents().contains(boundary.getRepository()))
				project.eResource().getContents().add(boundary.getRepository());
		}

		EclipseIDEService.saveProjectMetaData(project);

		rebuildAllFilesOfMethod(method);

		// Rebuild new or changed repositories
		for (final Repository repo : repositories)
			repositoryService.rebuildRepositorySourceFiles(repo);

		// Rebuild boundary source files
		for (final BoundaryBean boundary : boundarySet)
			boundaryService.rebuildBoundarySourceFiles(boundary);
	}

	/**
	 * @param mappingObject
	 */
	private void addMappingObjectToNamespace(ExchangeMappingObject mappingObject) {
		for (final Namespace n : project.getDTONamespace().getChildNamespaces())
			if (n.getName().equals(mappingObject.getDomainObject().getNamespace().getName())) {
				mappingObject.setNamespace(n);
				n.getJavaTypes().add(mappingObject);
				return;
			}
	}

	/**
	 * @param method
	 * @throws IllegalStateException if the XML schema file name isn't unique
	 */
	private void checkSchemaFileName(DataExchangeMethod method) {
		if (method.getSchemaFileName() == null || method.getSchemaFileName().isEmpty()
				|| method.getContentType() != ContentTypeEnumeration.XML)
			return;

		for (final Namespace ns : project.getExchangeNamespace().getChildNamespaces())
			for (final JavaType type : ns.getJavaTypes()) {
				if (!(type instanceof final DataExchangeServiceBean exchangeBean))
					continue;

				for (final DataExchangeMethod existingMethod : exchangeBean.getDataExchangeMethods()) {
					if (existingMethod.equals(method) || existingMethod.getContentType() != ContentTypeEnumeration.XML)
						continue;

					if (method.getSchemaFileName().equals(existingMethod.getSchemaFileName()))
						throw new IllegalStateException(
								"The data exchange method '" + existingMethod.getName() + "' has a XML schema file with the same name!");
				}
			}
	}

	/**
	 * Validate the method data
	 * @param method
	 * @throws IllegalStateException if the validation has failed
	 * @throws IllegalArgumentException if the method doesn't belong to a data exchange service
	 */
	private void validateMethodData(DataExchangeMethod method) {
		if (method.getDataExchangeServiceBean() == null)
			throw new IllegalArgumentException("A data exchange service for the given method must be defined!");

		validateMethodData(method, method.getDataExchangeServiceBean());
	}

	/**
	 * @param method
	 * @return the method's signature
	 */
	private String generateMethodSignature(DataExchangeMethod method) {
		final var identifier = new StringBuilder(method.getName());
		boolean firstParam = true;

		identifier.append("(");

		for (final MethodParameter param : method.getMethodParameters()) {
			if (firstParam)
				firstParam = false;
			else
				identifier.append(", ");

			if (param.getModifier() != JavaTypeModifierEnumeration.NONE) {
				identifier.append(param.getModifier().toString() + "<");
				identifier.append(param.getType().getName() + ">");
			}
			else
				identifier.append(param.getType().getName());

			identifier.append(" " + param.getName());
		}

		identifier.append(")");

		return identifier.toString();
	}

}

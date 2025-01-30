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
package net.codecadenza.eclipse.service.boundary;

import static net.codecadenza.eclipse.model.java.JavaType.STRING;
import static net.codecadenza.eclipse.model.java.JavaType.VOID;
import static net.codecadenza.eclipse.shared.Constants.BOUNDARY_BEAN_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.BOUNDARY_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.DTO_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.FACADE_BEAN_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.FACADE_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_COUNT;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_DELETE;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_FIND;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_SAVE;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_SEARCH;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.boundary.BoundaryBeanGenerator;
import net.codecadenza.eclipse.generator.boundary.BoundaryInterfaceGenerator;
import net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator;
import net.codecadenza.eclipse.generator.boundary.method.BoundaryMethodGeneratorUtility;
import net.codecadenza.eclipse.generator.facade.FacadeGenerator;
import net.codecadenza.eclipse.generator.facade.method.FacadeMethodGeneratorFactory;
import net.codecadenza.eclipse.generator.service.ServiceConfigurationGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.exchange.DataExchangeBeanService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Service for boundary beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryService {
	private static final Pattern FIND_PATTERN = Pattern.compile(METHOD_PREFIX_FIND);
	private static final Pattern SEARCH_PATTERN = Pattern.compile(METHOD_PREFIX_SEARCH);

	private final Project project;
	private final DTOBeanService dtoService;
	private final IntegrationBeanService integrationBeanService;
	private final RepositoryService repositoryService;

	/**
	 * Constructor
	 * @param project
	 */
	public BoundaryService(Project project) {
		this.project = project;
		this.dtoService = new DTOBeanService(project);
		this.integrationBeanService = new IntegrationBeanService(project);
		this.repositoryService = new RepositoryService(project);
	}

	/**
	 * Get a boundary and create it if it does not exist
	 * @param domainObject
	 * @return the boundary
	 * @throws Exception if an internal error has occurred
	 */
	public BoundaryBean getOrCreateBoundaryOfDomainObject(DomainObject domainObject) throws Exception {
		BoundaryBean boundary = project.getBoundaryByDomainObject(domainObject);

		if (boundary != null) {
			repositoryService.synchRepositoryMethods(boundary.getRepository());

			// Return the existing boundary
			return boundary;
		}

		// Create new boundary
		boundary = BoundaryFactory.eINSTANCE.createBoundaryBean();
		boundary.setDomainObject(domainObject);
		boundary.setPrimitive(false);
		boundary.setMappable(false);

		if (project.isBoundaryMode()) {
			boundary.setComment("Boundary service for " + domainObject.getLabel() + " objects");
			boundary.setInterfaceName(domainObject.getName() + BOUNDARY_SUFFIX);
			boundary.setName(domainObject.getName() + BOUNDARY_BEAN_SUFFIX);
		}
		else {
			boundary.setComment("Facade service for " + domainObject.getLabel() + " objects");
			boundary.setInterfaceName(domainObject.getName() + FACADE_SUFFIX);
			boundary.setName(domainObject.getName() + FACADE_BEAN_SUFFIX);
		}

		for (final Namespace ns : project.getBoundaryNamespace().getChildNamespaces())
			if (ns.getName().equals(domainObject.getNamespace().getName())) {
				boundary.setNamespace(ns);
				ns.getJavaTypes().add(boundary);
				break;
			}

		for (final Repository d : project.getAllRepositoriesOfProject()) {
			if (d.getDomainObject().equals(domainObject)) {
				boundary.setRepository(d);
				break;
			}
		}

		if (boundary.getRepository() == null) {
			// Create the repository for the given domain object
			final Repository repository = repositoryService.createRepository(domainObject);
			boundary.setRepository(repository);
		}

		// Rebuild the service configuration file
		new ServiceConfigurationGenerator(project).createFile();

		return boundary;
	}

	/**
	 * Check if the given boundary method already exists
	 * @param m
	 * @param boundaryBean
	 * @return the existing boundary method or null if the given boundary doesn't contain this method
	 * @throws IllegalStateException if a boundary method with same name and a different parameter list already exists
	 */
	public BoundaryMethod checkBoundaryMethod(BoundaryMethod m, BoundaryBean boundaryBean) {
		for (final BoundaryMethod existingMethod : boundaryBean.getBoundaryMethods()) {
			if (!existingMethod.getName().equals(m.getName()))
				continue;

			// Two methods with the same name and different method types are not allowed!
			if (existingMethod.getMethodType() != m.getMethodType())
				throw new IllegalStateException("A method with the name '" + m.getName() + "' and a different type already exists!");

			// We must set the method's bean reference in order to use the generator utility!
			m.setBoundaryBean(boundaryBean);

			// We must compare the boundary method signatures even if we work with facades!
			final String newSignature = new BoundaryMethodGeneratorUtility(m).getMethodSignature(false, false);
			final String existingSignature = new BoundaryMethodGeneratorUtility(existingMethod).getMethodSignature(false, false);

			// Reset the reference to the boundary bean in order to avoid saving this method!
			m.setBoundaryBean(null);

			// If both signatures are identical the method can be reused!
			if (newSignature.equals(existingSignature))
				return existingMethod;

			// Throw an exception even if the new method overwrites an existing method!
			throw new IllegalStateException("A method with the name '" + m.getName() + "' already exists!");
		}

		return null;
	}

	/**
	 * Rebuild the boundary source files
	 * @param boundaryBean
	 * @throws Exception if rebuilding of the boundary source files has failed
	 */
	public void rebuildBoundarySourceFiles(BoundaryBean boundaryBean) throws Exception {
		// Rebuild the boundary interface
		if (project.isAddBoundaryInterface())
			new BoundaryInterfaceGenerator(boundaryBean).createSourceFile();

		// Rebuild the boundary bean
		if (project.isBoundaryMode())
			new BoundaryBeanGenerator(boundaryBean).createSourceFile();
		else
			new FacadeGenerator(boundaryBean).createSourceFile();
	}

	/**
	 * Remove the given boundary bean
	 * @param boundaryBean
	 * @throws Exception if either the interface or the bean source file could not be deleted
	 * @throws IllegalStateException if the given boundary bean is referenced by other objects of the internal meta-model
	 */
	public void removeBoundary(BoundaryBean boundaryBean) throws Exception {
		final var forms = new ArrayList<String>();
		final var formPanels = new ArrayList<String>();
		final var outputName = project.isBoundaryMode() ? "boundary" : "facade";

		// Check if the boundary bean is used by either a form or a form action
		project.getAllFormsOfProject().forEach(form -> {
			if (form.getBoundaryMethod() != null && boundaryBean.equals(form.getBoundaryMethod().getBoundaryBean()))
				forms.add(form.getName() + " -> Method: " + form.getBoundaryMethod().getName());

			for (final FormAction action : form.getActions())
				if (action.getBoundaryMethod() != null && boundaryBean.equals(action.getBoundaryMethod().getBoundaryBean()))
					forms.add(form.getName() + " -> Action: " + action.getName());

			if (form.getFormType() == FormTypeEnumeration.TREE_VIEW) {
				final var treeView = (TreeView) form;

				if (treeView.getCountMethod() != null && boundaryBean.equals(treeView.getCountMethod().getBoundaryBean()))
					forms.add(treeView.getName() + " -> Method: " + treeView.getCountMethod().getName());

				if (treeView.getRecursiveMethod() != null && boundaryBean.equals(treeView.getRecursiveMethod().getBoundaryBean()))
					forms.add(treeView.getName() + " -> Method: " + treeView.getRecursiveMethod().getName());

				forms.addAll(searchBoundaryInTreeViewItem(treeView, boundaryBean, treeView.getRootTreeItem()));
			}
		});

		if (!forms.isEmpty()) {
			final var message = "The " + outputName + " cannot be removed, because it is referenced by following form(s):\n";

			throw new IllegalStateException(message + forms.stream().reduce((r, t) -> r + "\n" + t).orElse(""));
		}

		// Check if the boundary bean is used by either a grid panel or a grid panel action
		project.getAllGridPanelsOfProject().forEach(panel -> {
			if (panel.getBoundaryMethod() != null && boundaryBean.equals(panel.getBoundaryMethod().getBoundaryBean()))
				formPanels.add(panel.getName() + " -> Method: " + panel.getBoundaryMethod().getName());

			for (final FormAction action : panel.getActions())
				if (action.getBoundaryMethod() != null && boundaryBean.equals(action.getBoundaryMethod().getBoundaryBean()))
					formPanels.add(panel.getName() + " -> Action: " + action.getName());
		});

		if (!formPanels.isEmpty()) {
			final var message = "The " + outputName + " cannot be removed, because it is referenced by following grid panel(s):\n";

			throw new IllegalStateException(message + formPanels.stream().reduce((r, t) -> r + "\n" + t).orElse(""));
		}

		// Check if the boundary is used by an integration bean
		for (final IntegrationModule module : project.getIntegrationModules())
			for (final JavaType t : module.getNamespace().getJavaTypes()) {
				final var integrationBean = (AbstractIntegrationBean) t;

				if (integrationBeanService.getBoundariesOfIntegrationBean(integrationBean).contains(boundaryBean)) {
					final var message = "The " + outputName + " cannot be removed, because it is referenced by integration bean '"
							+ integrationBean.getName() + "'!";

					throw new IllegalStateException(message);
				}
			}

		if (!project.isBoundaryMode()) {
			final var dataExchangeBeanService = new DataExchangeBeanService(project);

			// Check if data exchange services depend on this facade
			for (final DataExchangeServiceBean exchangeService : project.getAllExchangeServices())
				if (dataExchangeBeanService.getAllRepositoriesOfExchangeService(exchangeService).contains(boundaryBean.getRepository())) {
					final var message = "The facade cannot be removed, because it is referenced by data exchange service '"
							+ exchangeService.getName() + "'!";

					throw new IllegalStateException(message);
				}

			// We should delete the respective repository as it cannot be removed by the user!
			for (final Repository c : project.getAllRepositoriesOfProject())
				if (c.getDomainObject().equals(boundaryBean.getDomainObject())) {
					project.eResource().getContents().remove(c);
					break;
				}
		}

		removeBoundarySourceFiles(boundaryBean);

		project.eResource().getContents().remove(boundaryBean);

		// Search for virtual data transfer objects that can be delete in the workspace
		dtoService.removeUnusedVirtualDTOs();

		EclipseIDEService.saveProjectMetaData(project);

		// We must reset the namespace of the boundary. Otherwise, it won't be removed in the service configuration file!
		boundaryBean.setNamespace(null);

		// Rebuild the service configuration file
		new ServiceConfigurationGenerator(project).createFile();
	}

	/**
	 * Remove the given boundary method
	 * @param boundaryMethod
	 * @throws Exception if the boundary method could not be removed
	 * @throws IllegalStateException if the given boundary method is referenced by other objects of the internal meta-model
	 */
	public void removeBoundaryMethod(BoundaryMethod boundaryMethod) throws Exception {
		final var forms = new BasicEList<String>();
		final var formPanels = new ArrayList<String>();
		final BoundaryBean b = boundaryMethod.getBoundaryBean();
		final var integrationBeans = new ArrayList<AbstractIntegrationBean>();

		// Check if the boundary method is used by either a form or a form action
		project.getAllFormsOfProject().forEach(form -> {
			if (boundaryMethod.equals(form.getBoundaryMethod()))
				forms.add(form.getName());

			for (final FormAction action : form.getActions())
				if (boundaryMethod.equals(action.getBoundaryMethod()))
					forms.add(form.getName() + " -> Action: " + action.getName());

			if (form.getFormType() == FormTypeEnumeration.TREE_VIEW) {
				final var treeView = (TreeView) form;

				if (boundaryMethod.equals(treeView.getCountMethod()))
					forms.add(treeView.getName());

				if (boundaryMethod.equals(treeView.getRecursiveMethod()))
					forms.add(treeView.getName());

				forms.addAll(searchBoundaryMethodInTreeViewItem(treeView, boundaryMethod, treeView.getRootTreeItem()));
			}
		});

		if (!forms.isEmpty()) {
			final var message = "The method cannot be removed, because it is referenced by following form(s):\n";
			final String formMsg = forms.stream().reduce((r, t) -> r + "\n" + t).orElse("");

			throw new IllegalStateException(message + formMsg);
		}

		// Check if the boundary method is used by either a grid panel or a grid panel action
		project.getAllGridPanelsOfProject().forEach(panel -> {
			if (boundaryMethod.equals(panel.getBoundaryMethod()))
				formPanels.add(panel.getName());

			for (final FormAction action : panel.getActions())
				if (boundaryMethod.equals(action.getBoundaryMethod()))
					formPanels.add(panel.getName() + " -> Action: " + action.getName());
		});

		if (!formPanels.isEmpty()) {
			final var message = "The method cannot be removed, because it is referenced by following grid panel(s):\n";
			final String formMsg = formPanels.stream().reduce((r, t) -> r + "\n" + t).orElse("");

			throw new IllegalStateException(message + formMsg);
		}

		// Check if the boundary method is used by an integration bean
		project.getIntegrationModules().forEach(module -> {
			// The integration module namespaces contain integration beans only!
			final Set<AbstractIntegrationBean> beans = module.getNamespace().getJavaTypes().stream()
					.map(AbstractIntegrationBean.class::cast).collect(Collectors.toSet());

			for (final AbstractIntegrationBean bean : beans)
				if (bean.getMethods().stream().anyMatch(m -> boundaryMethod.equals(m.getBoundaryMethod())))
					integrationBeans.add(bean);
		});

		if (!integrationBeans.isEmpty()) {
			final var message = "The method cannot be removed, because it is referenced by following integration bean(s):\n";
			final String beanNames = integrationBeans.stream().map(AbstractIntegrationBean::getName).reduce((r, t) -> r + "\n" + t)
					.orElse("");

			throw new IllegalStateException(message + beanNames);
		}

		b.getBoundaryMethods().remove(boundaryMethod);

		// Search for virtual data transfer objects that can be delete in the workspace
		dtoService.removeUnusedVirtualDTOs();

		EclipseIDEService.saveProjectMetaData(project);

		rebuildBoundarySourceFiles(b);
	}

	/**
	 * Add the change password boundary method
	 * @param boundary
	 * @param repositoryMethod
	 * @param logOnDTO
	 * @throws IllegalStateException if a boundary method with same name and a different parameter list already exists
	 */
	public void addChangePasswordMethodToBoundary(BoundaryBean boundary, RepositoryMethod repositoryMethod, DTOBean logOnDTO) {
		final BoundaryMethod changePasswordMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();

		changePasswordMethod.setName("changePassword");
		changePasswordMethod.setComment("Change password of given user account");
		changePasswordMethod.setReturnType(project.getJavaTypeByName(VOID));
		changePasswordMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		changePasswordMethod.setMethodType(BoundaryMethodTypeEnumeration.CHANGE_PASSWORD);
		changePasswordMethod.setServiceMethod(repositoryMethod);

		final MethodParameter paramId = JavaFactory.eINSTANCE.createMethodParameter();
		paramId.setName(logOnDTO.getPKAttribute().getDomainAttribute().getName());
		paramId.setType(logOnDTO.getPKAttribute().getDomainAttribute().getJavaType());
		paramId.setMethod(changePasswordMethod);
		paramId.setModifier(JavaTypeModifierEnumeration.NONE);
		changePasswordMethod.getMethodParameters().add(paramId);

		final MethodParameter paramOldPW = JavaFactory.eINSTANCE.createMethodParameter();
		paramOldPW.setName("oldPassword");
		paramOldPW.setType(project.getJavaTypeByName(STRING));
		paramOldPW.setMethod(changePasswordMethod);
		paramOldPW.setModifier(JavaTypeModifierEnumeration.NONE);
		changePasswordMethod.getMethodParameters().add(paramOldPW);

		final MethodParameter paramNewPassword = JavaFactory.eINSTANCE.createMethodParameter();
		paramNewPassword.setName("newPassword");
		paramNewPassword.setType(project.getJavaTypeByName(STRING));
		paramNewPassword.setMethod(changePasswordMethod);
		paramNewPassword.setModifier(JavaTypeModifierEnumeration.NONE);
		changePasswordMethod.getMethodParameters().add(paramNewPassword);

		final MethodParameter paramConfirmedPassword = JavaFactory.eINSTANCE.createMethodParameter();
		paramConfirmedPassword.setName("confirmedPassword");
		paramConfirmedPassword.setType(project.getJavaTypeByName(STRING));
		paramConfirmedPassword.setMethod(changePasswordMethod);
		paramConfirmedPassword.setModifier(JavaTypeModifierEnumeration.NONE);
		changePasswordMethod.getMethodParameters().add(paramConfirmedPassword);

		final BoundaryMethod existingMethod = checkBoundaryMethod(changePasswordMethod, boundary);

		if (existingMethod == null) {
			changePasswordMethod.setBoundaryBean(boundary);
			boundary.getBoundaryMethods().add(changePasswordMethod);
		}
	}

	/**
	 * Add the log-on boundary method
	 * @param boundary
	 * @param logOnRepositoryMethod
	 * @param logOnDTO
	 * @throws IllegalStateException if a boundary method with same name and a different parameter list already exists
	 */
	public void addLogOnMethodToBoundary(BoundaryBean boundary, RepositoryMethod logOnRepositoryMethod, DTOBean logOnDTO) {
		final BoundaryMethod logOnMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();

		logOnMethod.setName("logOn");
		logOnMethod.setComment("Perform user log-on");
		logOnMethod.setReturnType(logOnDTO);
		logOnMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		logOnMethod.setMethodType(BoundaryMethodTypeEnumeration.LOG_ON);
		logOnMethod.setServiceMethod(logOnRepositoryMethod);

		final MethodParameter paramName = JavaFactory.eINSTANCE.createMethodParameter();
		paramName.setName("userName");
		paramName.setType(project.getJavaTypeByName(STRING));
		paramName.setMethod(logOnMethod);
		paramName.setModifier(JavaTypeModifierEnumeration.NONE);
		logOnMethod.getMethodParameters().add(paramName);

		final MethodParameter paramPWD = JavaFactory.eINSTANCE.createMethodParameter();
		paramPWD.setName("password");
		paramPWD.setType(project.getJavaTypeByName(STRING));
		paramPWD.setMethod(logOnMethod);
		paramPWD.setModifier(JavaTypeModifierEnumeration.NONE);
		logOnMethod.getMethodParameters().add(paramPWD);

		final BoundaryMethod existingMethod = checkBoundaryMethod(logOnMethod, boundary);

		if (existingMethod == null) {
			logOnMethod.setBoundaryBean(boundary);
			boundary.getBoundaryMethods().add(logOnMethod);
		}
	}

	/**
	 * Search for new boundary beans to be created
	 * @return a set containing all new boundary beans
	 * @throws Exception if an internal error has occurred
	 */
	public Set<BoundaryBean> synchBoundaries() throws Exception {
		final var boundaries = new HashSet<BoundaryBean>();
		final EList<DomainObject> existingDomainObjects = project.getAllDomainObjectsOfProject(false, true);

		// Search for boundaries to be created
		for (final DomainObject domainObject : existingDomainObjects) {
			final BoundaryBean boundary = getOrCreateBoundaryOfDomainObject(domainObject);
			boundaries.add(boundary);
		}

		// Add the new boundaries to the meta-model
		boundaries.forEach(boundary -> {
			if (!project.eResource().getContents().contains(boundary))
				project.eResource().getContents().add(boundary);

			if (!project.eResource().getContents().contains(boundary.getRepository()))
				project.eResource().getContents().add(boundary.getRepository());
		});

		return boundaries;
	}

	/**
	 * Replace a given domain object parameter type by the type of the domain object's primary key attribute
	 * @param boundaryParam
	 * @param repositoryParam
	 */
	private void adaptParameterType(final MethodParameter boundaryParam, final MethodParameter repositoryParam) {
		if (!(repositoryParam.getType() instanceof final DomainObject domainObj))
			return;

		boundaryParam.setType(domainObj.getPKAttribute().getJavaType());
	}

	/**
	 * Create a new boundary method based on the given data exchange method
	 * @param dataExchangeMethod
	 * @return the boundary method
	 * @throws Exception if an internal error has occurred
	 */
	public BoundaryMethod initializeBoundaryMethod(final DataExchangeMethod dataExchangeMethod) throws Exception {
		final DomainObject domainObject = dataExchangeMethod.getDataExchangeServiceBean().getDomainObject();
		final BoundaryBean boundaryBean = getOrCreateBoundaryOfDomainObject(domainObject);

		// Create the boundary method
		final BoundaryMethod boundaryMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		boundaryMethod.setServiceMethod(dataExchangeMethod);
		boundaryMethod.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		boundaryMethod.setName(dataExchangeMethod.getName());
		boundaryMethod.setComment(dataExchangeMethod.getComment());
		boundaryMethod.setReturnType(project.getJavaTypeByName(VOID));

		if (dataExchangeMethod.getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT) {
			boundaryMethod.setMethodType(BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT);

			if (dataExchangeMethod.getExchangeMode() instanceof DirectExchangeMode) {
				// If a data exchange method uses the 'DIRECT' mode the corresponding boundary parameters will be created for all data
				// exchange parameters!
				dataExchangeMethod.getMethodParameters().forEach(exchangeParam -> {
					final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
					param.setName(exchangeParam.getName());
					param.setMethod(boundaryMethod);
					param.setType(exchangeParam.getType());

					boundaryMethod.getMethodParameters().add(param);
				});
			}
			else {
				// Check if a parameter should be added in order to select a specific object!
				final FilterMethodParameter singleObjectParam = dataExchangeMethod.getSingleObjectFilterParam();

				if (singleObjectParam != null) {
					final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
					param.setName(singleObjectParam.getName());
					param.setMethod(boundaryMethod);
					param.setType(singleObjectParam.getDomainAttribute().getJavaType());

					boundaryMethod.getMethodParameters().add(param);
				}
			}

			// Check if an export method either returns the absolute path, a content string or a mapping object
			if (dataExchangeMethod.getExchangeMode() instanceof DirectExchangeMode)
				boundaryMethod.setReturnType(dataExchangeMethod.getReturnType());
			else if (dataExchangeMethod.returnsPath() || dataExchangeMethod.returnsContent())
				boundaryMethod.setReturnType(project.getJavaTypeByName(STRING));
		}
		else {
			boundaryMethod.setMethodType(BoundaryMethodTypeEnumeration.UPLOAD_IMPORT);

			// Check if a parameter should be added that either represents an absolute path, a content string or a mapping object
			if (dataExchangeMethod.getMethodParameters().size() == 1) {
				final MethodParameter p = JavaFactory.eINSTANCE.createMethodParameter();
				p.setName(dataExchangeMethod.getMethodParameters().get(0).getName());
				p.setMethod(boundaryMethod);

				if (dataExchangeMethod.getExchangeMode() instanceof DirectExchangeMode)
					p.setType(dataExchangeMethod.getMethodParameters().get(0).getType());
				else
					p.setType(project.getJavaTypeByName(STRING));

				boundaryMethod.getMethodParameters().add(p);
			}
		}

		final BoundaryMethod existingMethod = checkBoundaryMethod(boundaryMethod, boundaryBean);

		if (existingMethod == null) {
			boundaryMethod.setBoundaryBean(boundaryBean);
			boundaryBean.getBoundaryMethods().add(boundaryMethod);

			return boundaryMethod;
		}

		return existingMethod;
	}

	/**
	 * Create a new boundary method based on the given repository method
	 * @param repository
	 * @param repositoryMethod
	 * @return the new boundary method
	 * @throws Exception if the initialization has failed
	 */
	public BoundaryMethod initializeBoundaryMethod(final Repository repository, final RepositoryMethod repositoryMethod)
			throws Exception {
		final RepositoryMethodTypeEnumeration repositoryType = repositoryMethod.getMethodType();
		final DomainObject domainObject = repository.getDomainObject();
		boolean firstParam = true;
		String methodName = repositoryMethod.getName();

		final BoundaryMethod boundaryMethod = BoundaryFactory.eINSTANCE.createBoundaryMethod();
		boundaryMethod.setComment(repositoryMethod.getComment());
		boundaryMethod.setServiceMethod(repositoryMethod);
		boundaryMethod.setMethodType(BoundaryMethodTypeEnumeration.valueOf(repositoryMethod.getMethodType().name()));
		boundaryMethod.setReturnTypeModifier(repositoryMethod.getReturnTypeModifier());

		if (!project.isBoundaryMode()) {
			// In case of special method types the method name must be changed in order to avoid name clashes!
			if (repositoryType == RepositoryMethodTypeEnumeration.DELETE)
				methodName = METHOD_PREFIX_DELETE + domainObject.getName();
			else if (repositoryType == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY
					|| repositoryType == RepositoryMethodTypeEnumeration.FIND_BY_ID
					|| repositoryType == RepositoryMethodTypeEnumeration.FIND_BY_OBJECT
					|| repositoryType == RepositoryMethodTypeEnumeration.FIND_EXISTING)
				methodName = FIND_PATTERN.matcher(methodName).replaceFirst(METHOD_PREFIX_FIND + domainObject.getName());
			else if (repositoryType == RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)
				methodName = SEARCH_PATTERN.matcher(methodName).replaceFirst(METHOD_PREFIX_SEARCH + domainObject.getNamePlural());
			else if (repositoryType == RepositoryMethodTypeEnumeration.FIND_ALL)
				methodName = METHOD_PREFIX_FIND + domainObject.getNamePlural();
			else if (repositoryType == RepositoryMethodTypeEnumeration.COUNT)
				methodName = METHOD_PREFIX_COUNT + domainObject.getNamePlural();
		}

		if (repositoryType == RepositoryMethodTypeEnumeration.SAVE) {
			methodName = METHOD_PREFIX_SAVE + domainObject.getName();
			boundaryMethod.setServiceMethod(repository.getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID));
		}

		boundaryMethod.setName(methodName);

		// For methods that either add or remove an object to an association the respective association field must be set!
		if (repositoryType == RepositoryMethodTypeEnumeration.ADD_TO_ASSOCIATION
				|| repositoryType == RepositoryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION) {
			for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
				for (final MethodParameter param : repositoryMethod.getMethodParameters()) {
					if (param.getHint() != null && param.getHint().equals(assoc.getName())) {
						boundaryMethod.setAssociation(assoc);
						break;
					}
				}
			}

			if (boundaryMethod.getAssociation() == null)
				throw new IllegalStateException("The association for the given boundary method was not initialized!");
		}

		// Initialize a basic query statement for 'COUNT' and 'SEARCH' methods
		if (repositoryType == RepositoryMethodTypeEnumeration.COUNT || repositoryType == RepositoryMethodTypeEnumeration.SEARCH) {
			boundaryMethod.setQueryStatement("from " + domainObject.getName() + " a ");
			boundaryMethod.setCustomStatement("");
		}

		if (repositoryMethod.getReturnType() instanceof final DomainObject returnTypeDomainObject) {
			boundaryMethod.setReturnType(findDTO(returnTypeDomainObject, repositoryType));

			// A 'COPY' method must return the value of the primary key attribute!
			if (repositoryType == RepositoryMethodTypeEnumeration.COPY)
				boundaryMethod.setReturnType(returnTypeDomainObject.getPKAttribute().getJavaType());
		}
		else if (project.getAllSupportedTypes().contains(repositoryMethod.getReturnType())
				|| repositoryMethod.getReturnType().isEnum())
			boundaryMethod.setReturnType(repositoryMethod.getReturnType());

		for (final MethodParameter param : repositoryMethod.getMethodParameters()) {
			// A 'COPY' method must not have more than one parameter!
			if (repositoryType == RepositoryMethodTypeEnumeration.COPY && !firstParam)
				break;

			final MethodParameter boundaryParam = JavaFactory.eINSTANCE.createMethodParameter();
			boundaryParam.setHint("");
			boundaryParam.setMethod(boundaryMethod);
			boundaryParam.setName(param.getName());
			boundaryParam.setModifier(param.getModifier());

			if (param.getType() instanceof final DomainObject paramDomainObject)
				boundaryParam.setType(findDTO(paramDomainObject, null));
			else if (project.getAllSupportedTypes().contains(param.getType()) || param.getType().isEnum())
				boundaryParam.setType(param.getType());

			// Adapt the parameter type if the method has a special type!
			if (repositoryType == RepositoryMethodTypeEnumeration.COPY
					|| repositoryType == RepositoryMethodTypeEnumeration.CHANGE_PARENT) {
				adaptParameterType(boundaryParam, param);

				// The parameter name of a 'COPY' method must be changed in order to avoid name clashes!
				if (repositoryType == RepositoryMethodTypeEnumeration.COPY)
					boundaryParam.setName(param.getName() + "Id");
			}
			else if (repositoryType == RepositoryMethodTypeEnumeration.ADD_TO_ASSOCIATION
					|| repositoryType == RepositoryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION)
				adaptParameterType(boundaryParam, param);

			boundaryMethod.getMethodParameters().add(boundaryParam);

			firstParam = false;
		}

		return boundaryMethod;
	}

	/**
	 * @param boundaryMethod
	 * @return the signature of the given boundary method
	 */
	public String getBoundaryMethodSignature(BoundaryMethod boundaryMethod) {
		return new BoundaryMethodGeneratorUtility(boundaryMethod).getMethodSignature(false, true);
	}

	/**
	 * @param boundaryMethod
	 * @return the signature of the given facade method. The method returns null if a corresponding facade method doesn't exist!
	 */
	public String getFacadeMethodSignature(BoundaryMethod boundaryMethod) {
		final BasicBoundaryMethodGenerator generator = FacadeMethodGeneratorFactory.getMethodGenerator(boundaryMethod, null);

		if (generator == null)
			return null;

		return generator.getMethodSignature(false);
	}

	/**
	 * Remove virtual facade methods that are not referenced by other objects of the meta-model
	 */
	public void removeUnusedVirtualMethods() {
		if (project.isBoundaryMode())
			return;

		final var methods = new ArrayList<BoundaryMethod>();

		// Search for all virtual facade methods
		project.getAllBoundariesOfProject()
				.forEach(boundary -> methods.addAll(boundary.getBoundaryMethods().stream().filter(BoundaryMethod::isVirtual).toList()));

		for (final BoundaryMethod method : methods) {
			boolean removeMethod = true;

			// Check if the facade method is used by a form
			for (final Form f : project.getAllFormsOfProject()) {
				if (f.getBoundaryMethod() != null && f.getBoundaryMethod().equals(method)) {
					removeMethod = false;
					break;
				}

				for (final FormAction a : f.getActions())
					if (a.getBoundaryMethod() != null && a.getBoundaryMethod().equals(method)) {
						removeMethod = false;
						break;
					}
			}

			if (removeMethod)
				removeMethod = !method.isUsedByIntegrationMethod();

			if (removeMethod) {
				final BoundaryBean boundaryBean = method.getBoundaryBean();
				boundaryBean.getBoundaryMethods().remove(method);
			}
		}
	}

	/**
	 * @param domainObject
	 * @param repositoryMethodType
	 * @return a DTO bean or null if an appropriate DTO couldn't be found
	 */
	private DTOBean findDTO(final DomainObject domainObject, final RepositoryMethodTypeEnumeration repositoryMethodType) {
		final EList<DTOBean> availableDTOs = project.getDTOsOfDomainObject(domainObject);

		if (repositoryMethodType != null && repositoryMethodType == RepositoryMethodTypeEnumeration.SEARCH) {
			// A manually created 'SEARCH' method must be supplied with a manually created DTO that in turn doesn't use standard
			// conversion!
			final Optional<DTOBean> dto = availableDTOs.stream().filter(e -> !e.isStandardConversion() && e.isCreatedManually())
					.findFirst();

			if (dto.isPresent())
				return dto.get();

			return null;
		}

		// Search for the default DTO that follows a predefined naming scheme!
		for (final DTOBean dto : availableDTOs)
			if (dto.getName().equals(dto.getDomainObject().getName() + DTO_SUFFIX))
				return dto;

		return availableDTOs.stream().findFirst().orElse(null);
	}

	/**
	 * Remove the boundary source files
	 * @param boundaryBean
	 * @throws Exception if either the interface or the bean source file could not be deleted
	 */
	private void removeBoundarySourceFiles(BoundaryBean boundaryBean) throws Exception {
		if (project.isAddBoundaryInterface()) {
			// Remove the interface
			EclipseIDEService.deleteSource(boundaryBean.getInterfaceSourceFile());
		}

		// Remove the bean
		EclipseIDEService.deleteSource(boundaryBean.getBeanSourceFile());

		if (boundaryBean.getTypeScriptSourceFile() != null)
			EclipseIDEService.deleteWorkspaceFile(boundaryBean.getTypeScriptSourceFile());
	}

	/**
	 * Search for references of the given boundary bean within a tree view item
	 * @param treeView
	 * @param boundaryBean
	 * @param treeViewItem
	 * @return a list that contains all references of the boundary bean
	 */
	private List<String> searchBoundaryInTreeViewItem(TreeView treeView, BoundaryBean boundaryBean, TreeViewItem treeViewItem) {
		final var itemList = new ArrayList<String>();

		if (treeViewItem.getDataFetchMethod() != null && boundaryBean.equals(treeViewItem.getDataFetchMethod().getBoundaryBean()))
			itemList.add(treeView.getName() + " -> Method: " + treeViewItem.getDataFetchMethod().getName());

		if (treeViewItem.getDropMethod() != null && boundaryBean.equals(treeViewItem.getDropMethod().getBoundaryBean()))
			itemList.add(treeView.getName() + " -> Method: " + treeViewItem.getDropMethod().getName());

		// Search for the boundary in all sub-items of this tree view item
		treeViewItem.getChildren().forEach(subItem -> itemList.addAll(searchBoundaryInTreeViewItem(treeView, boundaryBean, subItem)));

		return itemList;
	}

	/**
	 * Search for references of the given boundary method within a tree view item
	 * @param treeView
	 * @param boundaryMethod
	 * @param treeViewItem
	 * @return a list that contains all references of the boundary method
	 */
	private List<String> searchBoundaryMethodInTreeViewItem(TreeView treeView, BoundaryMethod boundaryMethod,
			TreeViewItem treeViewItem) {
		final var itemList = new ArrayList<String>();

		if (boundaryMethod.equals(treeViewItem.getDataFetchMethod()))
			itemList.add(treeView.getName());

		if (boundaryMethod.equals(treeViewItem.getDropMethod()))
			itemList.add(treeView.getName());

		// Search for the boundary method in all sub-items of this tree view item
		treeViewItem.getChildren()
				.forEach(subItem -> itemList.addAll(searchBoundaryMethodInTreeViewItem(treeView, boundaryMethod, subItem)));

		return itemList;
	}

}

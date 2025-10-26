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
package net.codecadenza.eclipse.service.repository;

import static net.codecadenza.eclipse.model.java.JavaType.BOOL;
import static net.codecadenza.eclipse.model.java.JavaType.LONG_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.VOID;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_ADD;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_CHANGE;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_EXIST;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_FIND;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_GET;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_REMOVE;
import static net.codecadenza.eclipse.shared.Constants.METHOD_PREFIX_SEARCH;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_CONTAIN;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_COPY;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_COUNT;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_DELETE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_DELETE_ALL;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_ALL;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_MERGE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_PERSIST;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_SAVE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_SEARCH;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import net.codecadenza.eclipse.generator.repository.RepositoryGenerator;
import net.codecadenza.eclipse.generator.repository.method.RepositoryMethodGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAssociationComparator;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeComparator;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryFactory;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.exchange.DataExchangeBeanService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Service for repositories
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryService {
	private static final String AND_NAME = "And";
	private static final String AND_CONSTANT = "_AND_";
	private static final String METHOD_PREFIX_EXISTS_BY = METHOD_PREFIX_EXIST + "By";
	private static final String METHOD_PREFIX_FIND_BY = METHOD_PREFIX_FIND + "By";
	private static final String METHOD_PREFIX_SEARCH_BY = METHOD_PREFIX_SEARCH + "By";
	private static final String PARAM_NAME_MUST_EXIST = "mustExist";
	private static final String PARAM_NAME_SOURCE_OBJ = "sourceObject";
	private static final String PARAM_NAME_TARGET_OBJ = "targetObject";

	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public RepositoryService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild the source file
	 * @param repository
	 * @throws Exception if the rebuild operation has failed
	 */
	public void rebuildRepositorySourceFiles(Repository repository) throws Exception {
		new RepositoryGenerator(repository).createSourceFile();
	}

	/**
	 * Remove the repository source file
	 * @param repository
	 * @throws Exception if the repository source file could not be deleted
	 */
	public void removeRepositorySourceFiles(Repository repository) throws Exception {
		EclipseIDEService.deleteSource(repository.getSourceFile());
	}

	/**
	 * @return a set containing either new or changed repositories
	 */
	public Set<Repository> synchRepositories() {
		// Get all existing repositories
		final EList<Repository> existingRepositories = project.getAllRepositoriesOfProject();
		final EList<DomainObject> allDomainObjects = project.getAllDomainObjectsOfProject(false, true);
		final var repositories = new HashSet<Repository>();

		// Search for new or changed repositories
		allDomainObjects.forEach(domainObject -> {
			final Repository existingRepo = existingRepositories.stream().filter(repo -> repo.getDomainObject().equals(domainObject))
					.findFirst().orElse(null);

			if (existingRepo != null) {
				final boolean changed = synchRepositoryMethods(existingRepo);

				if (changed)
					repositories.add(existingRepo);
			}
			else {
				// Create a new repository for the given domain object
				final Repository repository = createRepository(domainObject);

				// Add the repository to the meta-model
				project.eResource().getContents().add(repository);

				repositories.add(repository);
			}
		});

		return repositories;
	}

	/**
	 * @param existingRepository
	 * @return true if one or more methods have bean added
	 */
	public boolean synchRepositoryMethods(Repository existingRepository) {
		boolean changed = false;

		// Synchronize repository methods
		final EList<RepositoryMethod> methods = initializeMethods(existingRepository.getDomainObject());

		boolean methodFound = false;

		// Search for new methods to be added to the repository!
		for (final RepositoryMethod newMethod : methods) {
			methodFound = false;

			for (final RepositoryMethod existingMethod : existingRepository.getRepositoryMethods())
				// We must use a simple implementation to create the method's signature!
				if (createSimpleSignature(newMethod).equals(createSimpleSignature(existingMethod))) {
					methodFound = true;
					break;
				}

			if (!methodFound) {
				newMethod.setJavaType(existingRepository);
				newMethod.setRepository(existingRepository);

				existingRepository.getRepositoryMethods().add(newMethod);
				changed = true;
			}
		}

		return changed;
	}

	/**
	 * Create a new repository
	 * @param domainObject
	 * @return the created repository
	 */
	public Repository createRepository(DomainObject domainObject) {
		final Namespace repoParentNamespace = project.getRepositoryNamespace();
		final Optional<Namespace> repositoryNamespace = repoParentNamespace.getChildNamespaces().stream()
				.filter(nd -> nd.getName().equals(domainObject.getNamespace().getName())).findFirst();

		if (repositoryNamespace.isEmpty())
			throw new IllegalStateException(
					"The repository namespace '" + domainObject.getNamespace().getName() + "' could not be found!");

		// Initialize the repository
		final Repository repository = RepositoryFactory.eINSTANCE.createRepository();
		repository.setComment("Create, read, update and delete services for " + domainObject.getLabel() + " objects.");
		repository.setDomainObject(domainObject);

		final var repositoryName = domainObject.getName() + "Repository";

		repository.setName(repositoryName);
		repository.setPrimitive(false);
		repository.setMappable(false);
		repository.setNamespace(repositoryNamespace.get());

		final EList<RepositoryMethod> methods = initializeMethods(domainObject);

		// Add all methods to this repository
		methods.forEach(method -> {
			method.setJavaType(repository);
			method.setRepository(repository);

			repository.getRepositoryMethods().add(method);
		});

		// Add the repository to the respective namespace
		repositoryNamespace.get().getJavaTypes().add(repository);

		return repository;
	}

	/**
	 * @param method
	 * @return the signature of the method
	 */
	public String createSimpleSignature(RepositoryMethod method) {
		final var signature = new StringBuilder(method.getName());
		boolean isFirstParam1 = true;

		signature.append("(");

		for (final MethodParameter param : method.getMethodParameters()) {
			if (isFirstParam1) {
				if (param.getModifier() == JavaTypeModifierEnumeration.NONE)
					signature.append(param.getType().getName());
				else
					signature.append(param.getModifier().name() + "<" + param.getType().getName() + ">");

				isFirstParam1 = false;
			}
			else {
				signature.append(",");

				if (param.getModifier() == JavaTypeModifierEnumeration.NONE)
					signature.append(param.getType().getName());
				else
					signature.append(param.getModifier().name() + "<" + param.getType().getName() + ">");
			}
		}

		signature.append(")");

		return signature.toString();
	}

	/**
	 * Remove the given repository
	 * @param repository
	 * @throws Exception if the remove operation has failed
	 */
	public void removeRepository(Repository repository) throws Exception {
		final var dataExchangeBeanService = new DataExchangeBeanService(project);

		// Check if a boundary references the repository
		for (final BoundaryBean b : project.getAllBoundariesOfProject())
			if (b.getRepository().equals(repository)) {
				final var message = "The repository cannot be removed, because it is referenced by a respective boundary bean!";
				throw new IllegalStateException(message);
			}

		// Check if data exchange services depend on this repository
		for (final DataExchangeServiceBean exchangeService : project.getAllExchangeServices())
			if (dataExchangeBeanService.getAllRepositoriesOfExchangeService(exchangeService).contains(repository)) {
				final var message = "The repository cannot be removed, because it is referenced by data exchange service '"
						+ exchangeService.getName() + "'!";
				throw new IllegalStateException(message);
			}

		removeRepositorySourceFiles(repository);

		project.eResource().getContents().remove(repository);

		EclipseIDEService.saveProjectMetaData(project);
	}

	/**
	 * Remove the given repository method
	 * @param method
	 * @throws Exception if the repository method either could not be removed, or rebuilding of respective source files has failed
	 */
	public void removeRepositoryMethod(RepositoryMethod method) throws Exception {
		final Repository repository = method.getRepository();
		final BoundaryBean boundary = project.getBoundaryByDomainObject(repository.getDomainObject());

		// Test if the method is used by a boundary method
		if (boundary != null)
			for (final BoundaryMethod m : boundary.getBoundaryMethods())
				if (m.getServiceMethod().equals(method))
					throw new IllegalStateException(
							"The selected method cannot be removed as it is referenced by method '" + m.getName() + "()'!");

		repository.getRepositoryMethods().remove(method);
		project.eResource().getContents().remove(method);

		EclipseIDEService.saveProjectMetaData(project);

		if (project.isBoundaryMode())
			rebuildRepositorySourceFiles(repository);

		if (!project.isBoundaryMode() && boundary != null)
			new BoundaryService(project).rebuildBoundarySourceFiles(boundary);
	}

	/**
	 * Initialize all repository methods
	 * @param domainObject
	 * @return a list with repository methods
	 */
	public EList<RepositoryMethod> initializeMethods(DomainObject domainObject) {
		final DomainAttribute pkAttribute = domainObject.getPKAttribute();
		final var methods = new BasicEList<RepositoryMethod>();

		// Add the default find method
		final RepositoryMethod methodFind = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodFind.setName(REPO_METHOD_NAME_FIND_BY_ID);

		MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(pkAttribute.getJavaType());
		param.setName(pkAttribute.getName());
		param.setMethod(methodFind);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodFind.getMethodParameters().add(param);
		methodFind.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodFind.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodFind.setMethodType(RepositoryMethodTypeEnumeration.FIND_BY_ID);
		methodFind.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodFind.setReturnType(domainObject);

		methods.add(methodFind);

		// Add a find method with an additional parameter
		final RepositoryMethod methodFindExisting = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodFindExisting.setName(REPO_METHOD_NAME_FIND_BY_ID);

		param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(pkAttribute.getJavaType());
		param.setName(pkAttribute.getName());
		param.setMethod(methodFindExisting);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodFindExisting.getMethodParameters().add(param);

		// Add the 'mustExist' parameter
		param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(project.getJavaTypeByName(BOOL));
		param.setName(PARAM_NAME_MUST_EXIST);
		param.setMethod(methodFindExisting);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodFindExisting.getMethodParameters().add(param);
		methodFindExisting.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodFindExisting.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodFindExisting.setMethodType(RepositoryMethodTypeEnumeration.FIND_EXISTING);
		methodFindExisting.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodFindExisting.setReturnType(domainObject);

		methods.add(methodFindExisting);

		// Add a finder method to search an entity by using an existing one
		final RepositoryMethod methodObjectFind = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodObjectFind.setName(REPO_METHOD_NAME_FIND_BY_ID);

		param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(domainObject);
		param.setName(domainObject.getLowerCaseName());
		param.setMethod(methodObjectFind);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodObjectFind.getMethodParameters().add(param);
		methodObjectFind.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodObjectFind.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodObjectFind.setMethodType(RepositoryMethodTypeEnumeration.FIND_BY_OBJECT);
		methodObjectFind.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodObjectFind.setReturnType(domainObject);

		methods.add(methodObjectFind);

		// Add a method to remove all persistent objects
		final RepositoryMethod methodDeleteAll = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodDeleteAll.setName(REPO_METHOD_NAME_DELETE_ALL);
		methodDeleteAll.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodDeleteAll.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodDeleteAll.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodDeleteAll.setMethodType(RepositoryMethodTypeEnumeration.DELETE_ALL);
		methodDeleteAll.setReturnType(project.getJavaTypeByName(VOID));

		methods.add(methodDeleteAll);

		// Add a method to merge an entity
		if (!domainObject.isAbstract()) {
			final RepositoryMethod methodMerge = RepositoryFactory.eINSTANCE.createRepositoryMethod();
			methodMerge.setName(REPO_METHOD_NAME_MERGE);
			methodMerge.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
			methodMerge.setTransactionType(TransactionTypeEnumeration.REQUIRED);
			methodMerge.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			methodMerge.setMethodType(RepositoryMethodTypeEnumeration.MERGE);
			methodMerge.setReturnType(domainObject);

			param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setType(domainObject);
			param.setName(domainObject.getLowerCaseName());
			param.setMethod(methodMerge);
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			methodMerge.getMethodParameters().add(param);

			methods.add(methodMerge);
		}

		if (!domainObject.isAbstract()) {
			// Add a method to persist an entity
			final RepositoryMethod methodPersist = RepositoryFactory.eINSTANCE.createRepositoryMethod();
			methodPersist.setName(REPO_METHOD_NAME_PERSIST);
			methodPersist.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
			methodPersist.setTransactionType(TransactionTypeEnumeration.REQUIRED);
			methodPersist.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			methodPersist.setMethodType(RepositoryMethodTypeEnumeration.PERSIST);
			methodPersist.setReturnType(domainObject);

			param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setType(domainObject);
			param.setName(domainObject.getLowerCaseName());
			param.setMethod(methodPersist);
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			methodPersist.getMethodParameters().add(param);

			methods.add(methodPersist);

			// Add a method to copy an entity
			final RepositoryMethod methodCopy = RepositoryFactory.eINSTANCE.createRepositoryMethod();
			methodCopy.setName(REPO_METHOD_NAME_COPY);
			methodCopy.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
			methodCopy.setTransactionType(TransactionTypeEnumeration.REQUIRED);
			methodCopy.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
			methodCopy.setMethodType(RepositoryMethodTypeEnumeration.COPY);
			methodCopy.setReturnType(domainObject);

			param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setType(domainObject);
			param.setName(PARAM_NAME_SOURCE_OBJ);
			param.setMethod(methodCopy);
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			methodCopy.getMethodParameters().add(param);

			param = JavaFactory.eINSTANCE.createMethodParameter();
			param.setType(domainObject);
			param.setName(PARAM_NAME_TARGET_OBJ);
			param.setMethod(methodCopy);
			param.setModifier(JavaTypeModifierEnumeration.NONE);

			methodCopy.getMethodParameters().add(param);

			methods.add(methodCopy);
		}

		// Add a method to delete an entity
		final RepositoryMethod methodDelete = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodDelete.setName(REPO_METHOD_NAME_DELETE);
		methodDelete.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodDelete.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodDelete.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodDelete.setMethodType(RepositoryMethodTypeEnumeration.DELETE);
		methodDelete.setReturnType(project.getJavaTypeByName(VOID));

		param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(pkAttribute.getJavaType());
		param.setName(pkAttribute.getName());
		param.setMethod(methodDelete);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodDelete.getMethodParameters().add(param);

		methods.add(methodDelete);

		// Add a finder method to get all entities
		final RepositoryMethod methodFindAll = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodFindAll.setName(REPO_METHOD_NAME_FIND_ALL);
		methodFindAll.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodFindAll.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodFindAll.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		methodFindAll.setMethodType(RepositoryMethodTypeEnumeration.FIND_ALL);
		methodFindAll.setReturnType(domainObject);

		methods.add(methodFindAll);

		// Add the unique key exist methods
		for (final DomainObject domainObjectOfTree : domainObject.getFullInheritanceTree()) {
			if (domainObjectOfTree.isMappedSuperClass() || domainObjectOfTree.getDatabaseTable() == null)
				continue;

			// In the case of a joined inheritance strategy it is possible that the unique keys are distributed over different tables!
			for (final DBIndex dbIndex : domainObjectOfTree.getDatabaseTable().getIndexes()) {
				if (!dbIndex.isUnique() || dbIndex.getColumns().size() > 2)
					continue;

				methods.addAll(createUniqueKeyMethods(domainObject, dbIndex));
			}
		}

		// Add a method that checks if an entity already exists
		final RepositoryMethod methodExistsById = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodExistsById.setName(REPO_METHOD_NAME_CONTAIN);

		param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(pkAttribute.getJavaType());
		param.setName(pkAttribute.getName());
		param.setMethod(methodExistsById);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodExistsById.getMethodParameters().add(param);
		methodExistsById.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodExistsById.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodExistsById.setMethodType(RepositoryMethodTypeEnumeration.EXISTS_BY_ID);
		methodExistsById.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodExistsById.setReturnType(project.getJavaTypeByName(BOOL));

		methods.add(methodExistsById);

		// Add a method to count all entities
		final RepositoryMethod methodCountAll = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodCountAll.setName(REPO_METHOD_NAME_COUNT);
		methodCountAll.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodCountAll.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodCountAll.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodCountAll.setMethodType(RepositoryMethodTypeEnumeration.COUNT_ALL);
		methodCountAll.setReturnType(project.getJavaTypeByName(LONG_OBJ));

		methods.add(methodCountAll);

		// Add the default search method. Note that no parameters are added here as they are always the same and the generator takes
		// care of a proper method signature!
		final RepositoryMethod methodSearch = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodSearch.setName(REPO_METHOD_NAME_SEARCH);
		methodSearch.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodSearch.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodSearch.setMethodType(RepositoryMethodTypeEnumeration.SEARCH);
		methodSearch.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		methodSearch.setReturnType(domainObject);

		methods.add(methodSearch);

		// Add the default count method. Note that no parameters are added here as they are always the same and the generator takes
		// care of a proper method signature!
		final RepositoryMethod methodCount = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodCount.setName(REPO_METHOD_NAME_COUNT);
		methodCount.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodCount.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodCount.setMethodType(RepositoryMethodTypeEnumeration.COUNT);
		methodCount.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodCount.setReturnType(project.getJavaTypeByName(LONG_OBJ));

		methods.add(methodCount);

		for (final DomainObject bean : domainObject.getFullInheritanceTree()) {
			// Add 'GET_ASSOCIATION' methods only for projects that use boundaries!
			if (project.isBoundaryMode())
				for (final AbstractDomainAssociation association : bean.getAssociations()) {
					// Add a 'GET_ASSOCIATION' method for all associations even if the association is not annotated respectively!
					final RepositoryMethod methodGetAssociation = RepositoryFactory.eINSTANCE.createRepositoryMethod();
					methodGetAssociation.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
					methodGetAssociation.setTransactionType(TransactionTypeEnumeration.REQUIRED);
					methodGetAssociation.setMethodType(RepositoryMethodTypeEnumeration.GET_ASSOCIATION);
					methodGetAssociation.setHint("NQ_GET_" + association.getName().toUpperCase());

					final RepositoryMethodParameter repositoryParam = RepositoryFactory.eINSTANCE.createRepositoryMethodParameter();
					repositoryParam.setType(pkAttribute.getJavaType());
					repositoryParam.setName(pkAttribute.getName());
					repositoryParam.setMethod(methodGetAssociation);
					repositoryParam.setModifier(JavaTypeModifierEnumeration.NONE);
					repositoryParam.setAssociation(association);

					methodGetAssociation.getMethodParameters().add(repositoryParam);
					methodGetAssociation.setReturnType(association.getTarget());
					methodGetAssociation.setName(METHOD_PREFIX_GET + association.getUpperCaseName());

					if (association instanceof OneToManyAssociation || association instanceof ManyToManyAssociation)
						methodGetAssociation.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
					else
						methodGetAssociation.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);

					methods.add(methodGetAssociation);
				}

			// Do not add the following methods to the repository if the domain object is abstract!
			if (domainObject.isAbstract())
				continue;

			// Add methods for changing many-to-one associations
			for (final AbstractDomainAssociation association : bean.getAssociations()) {
				if (!(association instanceof final ManyToOneAssociation mto))
					continue;

				if (!mto.isUpdatable())
					continue;

				final RepositoryMethod methodChangeParent = RepositoryFactory.eINSTANCE.createRepositoryMethod();
				methodChangeParent.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
				methodChangeParent.setTransactionType(TransactionTypeEnumeration.REQUIRED);
				methodChangeParent.setMethodType(RepositoryMethodTypeEnumeration.CHANGE_PARENT);
				methodChangeParent.setName(METHOD_PREFIX_CHANGE + mto.getUpperCaseName());

				param = JavaFactory.eINSTANCE.createMethodParameter();
				param.setType(pkAttribute.getJavaType());
				param.setName(pkAttribute.getName());
				param.setMethod(methodChangeParent);
				param.setModifier(JavaTypeModifierEnumeration.NONE);

				methodChangeParent.getMethodParameters().add(param);

				final RepositoryMethodParameter repositoryParam = RepositoryFactory.eINSTANCE.createRepositoryMethodParameter();
				repositoryParam.setType(mto.getTarget());
				repositoryParam.setName(mto.getName());
				repositoryParam.setAssociation(mto);
				repositoryParam.setMethod(methodChangeParent);
				repositoryParam.setModifier(JavaTypeModifierEnumeration.NONE);

				methodChangeParent.getMethodParameters().add(repositoryParam);
				methodChangeParent.setReturnType(project.getJavaTypeByName(VOID));
				methodChangeParent.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);

				methods.add(methodChangeParent);
			}

			// Add methods for maintaining many-to-many and unidirectional one-to-many associations
			for (final AbstractDomainAssociation assoc : bean.getAssociations()) {
				if (!assoc.isOwner())
					continue;

				if (assoc instanceof ManyToManyAssociation || assoc instanceof final OneToManyAssociation otm && !otm.isBidirectional()) {
					final RepositoryMethod methodRemoveFromAssoc = RepositoryFactory.eINSTANCE.createRepositoryMethod();
					methodRemoveFromAssoc.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
					methodRemoveFromAssoc.setTransactionType(TransactionTypeEnumeration.REQUIRED);
					methodRemoveFromAssoc.setMethodType(RepositoryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION);
					methodRemoveFromAssoc.setName(METHOD_PREFIX_REMOVE + assoc.getTarget().getName() + "From" + assoc.getUpperCaseName());

					param = JavaFactory.eINSTANCE.createMethodParameter();
					param.setType(pkAttribute.getJavaType());
					param.setName(pkAttribute.getName());
					param.setMethod(methodRemoveFromAssoc);
					param.setModifier(JavaTypeModifierEnumeration.NONE);

					methodRemoveFromAssoc.getMethodParameters().add(param);

					param = JavaFactory.eINSTANCE.createMethodParameter();
					param.setType(assoc.getTarget());
					param.setName(assoc.getTarget().getLowerCaseName());
					param.setMethod(methodRemoveFromAssoc);
					param.setModifier(JavaTypeModifierEnumeration.NONE);
					param.setHint(assoc.getName());

					methodRemoveFromAssoc.getMethodParameters().add(param);
					methodRemoveFromAssoc.setReturnType(project.getJavaTypeByName(VOID));
					methodRemoveFromAssoc.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);

					methods.add(methodRemoveFromAssoc);
				}

				if (assoc instanceof ManyToManyAssociation) {
					final RepositoryMethod methodAddToAssoc = RepositoryFactory.eINSTANCE.createRepositoryMethod();
					methodAddToAssoc.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
					methodAddToAssoc.setTransactionType(TransactionTypeEnumeration.REQUIRED);
					methodAddToAssoc.setMethodType(RepositoryMethodTypeEnumeration.ADD_TO_ASSOCIATION);
					methodAddToAssoc.setName(METHOD_PREFIX_ADD + assoc.getTarget().getName() + "To" + assoc.getUpperCaseName());

					param = JavaFactory.eINSTANCE.createMethodParameter();
					param.setType(pkAttribute.getJavaType());
					param.setName(pkAttribute.getName());
					param.setMethod(methodAddToAssoc);
					param.setModifier(JavaTypeModifierEnumeration.NONE);

					methodAddToAssoc.getMethodParameters().add(param);

					param = JavaFactory.eINSTANCE.createMethodParameter();
					param.setType(assoc.getTarget());
					param.setName(assoc.getTarget().getLowerCaseName());
					param.setMethod(methodAddToAssoc);
					param.setModifier(JavaTypeModifierEnumeration.NONE);
					param.setHint(assoc.getName());

					methodAddToAssoc.getMethodParameters().add(param);
					methodAddToAssoc.setReturnType(project.getJavaTypeByName(VOID));
					methodAddToAssoc.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);

					methods.add(methodAddToAssoc);
				}
			}
		}

		return methods;
	}

	/**
	 * Initialize a 'SAVE' method
	 * @param repository
	 * @return the 'SAVE' method
	 */
	public RepositoryMethod initializeSaveMethod(Repository repository) {
		final DomainObject domainObject = repository.getDomainObject();

		final RepositoryMethod methodSave = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		methodSave.setName(REPO_METHOD_NAME_SAVE);

		final MethodParameter param = JavaFactory.eINSTANCE.createMethodParameter();
		param.setType(domainObject);
		param.setName(domainObject.getLowerCaseName());
		param.setMethod(methodSave);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		methodSave.getMethodParameters().add(param);
		methodSave.setMethodType(RepositoryMethodTypeEnumeration.SAVE);
		methodSave.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodSave.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodSave.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodSave.setReturnType(domainObject);
		methodSave.setComment("Save the given " + domainObject.getLabel());

		return methodSave;
	}

	/**
	 * @param repositoryMethod
	 * @return the signature of the given repository method
	 */
	public String getMethodSignature(RepositoryMethod repositoryMethod) {
		return RepositoryMethodGeneratorFactory.getMethodGenerator(repositoryMethod, null).getMethodSignature();
	}

	/**
	 * Create methods of type 'EXISTS_BY_UNIQUE_KEY', 'EXISTS_BY_UNIQUE_KEY_WITH_ID', 'FIND_BY_UNIQUE_KEY' and
	 * 'SEARCH_BY_UNIQUE_KEY' based on the provided unique key
	 * @param domainObject
	 * @param uniqueKey
	 * @return a list that contains the created repository methods
	 */
	private List<RepositoryMethod> createUniqueKeyMethods(DomainObject domainObject, DBIndex uniqueKey) {
		final var methods = new BasicEList<RepositoryMethod>();
		int checkCounter = 0;
		boolean addSearchMethod = false;
		DomainAttribute attr1 = null;
		DomainAttribute attr2 = null;
		AbstractDomainAssociation assoc1 = null;
		AbstractDomainAssociation assoc2 = null;

		// Sort all attributes in order to be in sync with the naming in respective domain objects!
		final var attributes = new BasicEList<DomainAttribute>();
		attributes.addAll(domainObject.getAllAttributes());

		ECollections.sort(attributes, new DomainAttributeComparator());

		// Sort all associations in order to be in sync with the naming in respective domain objects!
		final var assocs = new BasicEList<AbstractDomainAssociation>();
		assocs.addAll(domainObject.getAllAssociations());

		ECollections.sort(assocs, new DomainAssociationComparator());

		// Check if unique key methods can be added
		for (final DBColumn column : uniqueKey.getColumns())
			for (final DomainAttribute attribute : attributes)
				if (attribute.getColumn() != null && attribute.getColumn().equals(column)) {
					checkCounter++;

					if (attr1 == null)
						attr1 = attribute;
					else
						attr2 = attribute;

					if (attribute.getJavaType().isString())
						addSearchMethod = true;
				}

		for (final DBColumn col : uniqueKey.getColumns())
			for (final AbstractDomainAssociation assoc : assocs) {
				if ((assoc instanceof final ManyToOneAssociation manyToOne && col.equals(manyToOne.getColumn()))
						|| (assoc instanceof final OneToOneAssociation oneToOne && oneToOne.isOwner() && col.equals(oneToOne.getColumn()))) {
					if (assoc1 == null)
						assoc1 = assoc;
					else
						assoc2 = assoc;

					checkCounter++;
				}
			}

		if (checkCounter != uniqueKey.getColumns().size())
			return methods;

		final RepositoryMethod methodExistsByUK = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		final RepositoryMethod methodExistsByUKAndId = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		final RepositoryMethod methodSearchByUK = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		final RepositoryMethod methodFindByUK = RepositoryFactory.eINSTANCE.createRepositoryMethod();
		var nameExists = METHOD_PREFIX_EXISTS_BY;
		var nameExistsWithId = METHOD_PREFIX_EXISTS_BY + domainObject.getPKAttribute().getUpperCaseName() + AND_NAME;
		var nameSearch = METHOD_PREFIX_SEARCH_BY;
		var nameFind = METHOD_PREFIX_FIND_BY;
		var constantExists = "";
		var constantExistsWithId = "";
		var constantSearch = "";
		var constantFind = "";

		// A method of type 'EXISTS_BY_UNIQUE_KEY_WITH_ID' requires a parameter that is mapped to the primary key attribute!
		addParameterToUniqueKeyMethod(methodExistsByUKAndId, domainObject.getPKAttribute());

		boolean isFirstParameter = true;

		if (attr1 != null) {
			addParameterToUniqueKeyMethod(methodExistsByUKAndId, attr1);
			addParameterToUniqueKeyMethod(methodExistsByUK, attr1);
			addParameterToUniqueKeyMethod(methodSearchByUK, attr1);
			addParameterToUniqueKeyMethod(methodFindByUK, attr1);

			isFirstParameter = false;
			constantExists = attr1.getName();
			constantSearch = attr1.getName();
			constantFind = attr1.getName();
			constantExistsWithId = attr1.getName();

			nameExists += attr1.getUpperCaseName();
			nameSearch += attr1.getUpperCaseName();
			nameFind += attr1.getUpperCaseName();
			nameExistsWithId += attr1.getUpperCaseName();
		}

		if (attr2 != null) {
			addParameterToUniqueKeyMethod(methodExistsByUKAndId, attr2);
			addParameterToUniqueKeyMethod(methodExistsByUK, attr2);
			addParameterToUniqueKeyMethod(methodSearchByUK, attr2);
			addParameterToUniqueKeyMethod(methodFindByUK, attr2);

			isFirstParameter = false;
			constantExists += AND_CONSTANT + attr2.getName();
			constantSearch += AND_CONSTANT + attr2.getName();
			constantFind += AND_CONSTANT + attr2.getName();
			constantExistsWithId += AND_CONSTANT + attr2.getName();

			nameExists += AND_NAME + attr2.getUpperCaseName();
			nameSearch += AND_NAME + attr2.getUpperCaseName();
			nameFind += AND_NAME + attr2.getUpperCaseName();
			nameExistsWithId += AND_NAME + attr2.getUpperCaseName();
		}

		if (assoc1 != null) {
			final String idAttrName = assoc1.getDomainObject().getPKAttribute().getUpperCaseName();
			String andConstant = "";
			String andName = "";

			addParameterToUniqueKeyMethod(methodExistsByUKAndId, assoc1);
			addParameterToUniqueKeyMethod(methodExistsByUK, assoc1);
			addParameterToUniqueKeyMethod(methodSearchByUK, assoc1);
			addParameterToUniqueKeyMethod(methodFindByUK, assoc1);

			if (!isFirstParameter) {
				andConstant = AND_CONSTANT;
				andName = AND_NAME;
			}

			constantExists += andConstant + assoc1.getName();
			constantSearch += andConstant + assoc1.getName();
			constantFind += andConstant + assoc1.getName();
			constantExistsWithId += andConstant + assoc1.getName();

			nameExists += andName + assoc1.getUpperCaseName() + idAttrName;
			nameSearch += andName + assoc1.getUpperCaseName() + idAttrName;
			nameFind += andName + assoc1.getUpperCaseName() + idAttrName;
			nameExistsWithId += andName + assoc1.getUpperCaseName() + idAttrName;
		}

		if (assoc2 != null) {
			final String idAttrName = assoc2.getDomainObject().getPKAttribute().getUpperCaseName();

			addParameterToUniqueKeyMethod(methodExistsByUKAndId, assoc2);
			addParameterToUniqueKeyMethod(methodExistsByUK, assoc2);
			addParameterToUniqueKeyMethod(methodSearchByUK, assoc2);
			addParameterToUniqueKeyMethod(methodFindByUK, assoc2);

			constantExists += AND_CONSTANT + assoc2.getName();
			constantSearch += AND_CONSTANT + assoc2.getName();
			constantFind += AND_CONSTANT + assoc2.getName();
			constantExistsWithId += AND_CONSTANT + assoc2.getName();

			nameExists += AND_NAME + assoc2.getUpperCaseName() + idAttrName;
			nameSearch += AND_NAME + assoc2.getUpperCaseName() + idAttrName;
			nameFind += AND_NAME + assoc2.getUpperCaseName() + idAttrName;
			nameExistsWithId += AND_NAME + assoc2.getUpperCaseName() + idAttrName;
		}

		methodExistsByUK.setHint("NQ_UK_EXISTS_BY_" + constantExists.toUpperCase());
		methodExistsByUK.setName(nameExists);
		methodExistsByUK.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodExistsByUK.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodExistsByUK.setMethodType(RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY);
		methodExistsByUK.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodExistsByUK.setReturnType(project.getJavaTypeByName(BOOL));

		methods.add(methodExistsByUK);

		constantExistsWithId += AND_CONSTANT + domainObject.getPKAttribute().getName();

		methodExistsByUKAndId.setHint("NQ_UK_EXISTS_BY_" + constantExistsWithId.toUpperCase());
		methodExistsByUKAndId.setName(nameExistsWithId);
		methodExistsByUKAndId.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodExistsByUKAndId.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodExistsByUKAndId.setMethodType(RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID);
		methodExistsByUKAndId.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodExistsByUKAndId.setReturnType(project.getJavaTypeByName(BOOL));

		methods.add(methodExistsByUKAndId);

		methodSearchByUK.setHint("NQ_UK_SEARCH_BY_" + constantSearch.toUpperCase());
		methodSearchByUK.setName(nameSearch);
		methodSearchByUK.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodSearchByUK.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodSearchByUK.setMethodType(RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY);
		methodSearchByUK.setReturnTypeModifier(JavaTypeModifierEnumeration.LIST);
		methodSearchByUK.setReturnType(domainObject);

		if (addSearchMethod)
			methods.add(methodSearchByUK);

		methodFindByUK.setHint("NQ_UK_FIND_BY_" + constantFind.toUpperCase());
		methodFindByUK.setName(nameFind);
		methodFindByUK.setPermissionMode(PermissionModeEnumeration.PERMIT_ALL);
		methodFindByUK.setTransactionType(TransactionTypeEnumeration.REQUIRED);
		methodFindByUK.setMethodType(RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY);
		methodFindByUK.setReturnTypeModifier(JavaTypeModifierEnumeration.NONE);
		methodFindByUK.setReturnType(domainObject);

		methods.add(methodFindByUK);

		return methods;
	}

	/**
	 * Add a parameter to the given repository method based on the provided domain attribute
	 * @param repositoryMethod
	 * @param attribute
	 */
	private void addParameterToUniqueKeyMethod(RepositoryMethod repositoryMethod, DomainAttribute attribute) {
		final RepositoryMethodParameter param = RepositoryFactory.eINSTANCE.createRepositoryMethodParameter();
		param.setType(attribute.getJavaType());
		param.setName(attribute.getName());
		param.setMethod(repositoryMethod);
		param.setModifier(JavaTypeModifierEnumeration.NONE);
		param.setAttribute(attribute);

		repositoryMethod.getMethodParameters().add(param);
	}

	/**
	 * Add a parameter to the given repository method based on the provided domain association
	 * @param repositoryMethod
	 * @param assoc
	 */
	private void addParameterToUniqueKeyMethod(RepositoryMethod repositoryMethod, AbstractDomainAssociation assoc) {
		final RepositoryMethodParameter param = RepositoryFactory.eINSTANCE.createRepositoryMethodParameter();
		final JavaType pkType = assoc.getTarget().getPKAttribute().getJavaType();

		if ((assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
				|| (assoc instanceof final OneToOneAssociation oto && oto.isOptional())) {
			// The parameter for an optional association must be nullable!
			param.setType(project.getJavaTypeByName(pkType.getWrapperTypeName()));
		}
		else
			param.setType(pkType);

		param.setName(assoc.getName() + assoc.getTarget().getPKAttribute().getUpperCaseName());
		param.setMethod(repositoryMethod);
		param.setAssociation(assoc);
		param.setModifier(JavaTypeModifierEnumeration.NONE);

		repositoryMethod.getMethodParameters().add(param);
	}

}

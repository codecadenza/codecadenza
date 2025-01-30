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
package net.codecadenza.eclipse.generator.boundary.method;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_SEARCH;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.CommonGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;

/**
 * <p>
 * Basic generator for boundary and facade methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicBoundaryMethodGenerator {
	protected BoundaryMethod method;
	protected boolean addTransactionManagement;
	protected boolean addTransactionAnnotation;
	protected Project project;
	protected String domainObjectLabel = "";
	protected String domainObjectPluralLabel = "";
	protected String clientParamComment;
	protected String validationExceptionComment = "";
	protected String validationExceptionImport = "";
	protected IMethodGeneratorUtility generatorUtility;
	protected AbstractJavaSourceGenerator parentGenerator;

	/**
	 * Constructor
	 * @param method
	 * @param generatorUtility
	 * @param parentGenerator
	 */
	public BasicBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility generatorUtility,
			AbstractJavaSourceGenerator parentGenerator) {
		this.method = method;
		this.project = method.getBoundaryBean().getNamespace().getProject();
		this.generatorUtility = generatorUtility;
		this.parentGenerator = parentGenerator;
		this.domainObjectLabel = method.getBoundaryBean().getDomainObject().getLabel();
		this.domainObjectPluralLabel = method.getBoundaryBean().getDomainObject().getLabelPlural();

		if (!project.isJakartaEEApplication()) {
			if (method.getMethodType() == BoundaryMethodTypeEnumeration.UPDATE
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.CREATE
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.CHANGE_PARENT
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.CHANGE_ASSOCIATION
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.DELETE
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.DELETE_ALL
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.CHANGE_PASSWORD
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.COPY
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.SAVE) {
				if (project.isSpringBootApplication())
					addTransactionAnnotation = true;
				else
					addTransactionManagement = true;
			}

			if (project.isSpringBootApplication() && (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_ID
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_OBJECT
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_EXISTING
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_ALL
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.LOG_ON))
				addTransactionAnnotation = true;
		}

		if (project.getValidationType() == ValidationTypeEnumeration.INTERNAL) {
			validationExceptionComment = " * @throws PropertyConstraintViolationException if the validation of one or more persistent attribute values has failed\n";
			validationExceptionImport = "import net.codecadenza.runtime.validation.PropertyConstraintViolationException;";
		}
		else if (project.isJavaSEApplication() || project.hasJSFOrVaadinClient()) {
			validationExceptionComment = " * @throws ConstraintViolationException if the validation of one or more persistent attribute values has failed\n";
			validationExceptionImport = "import jakarta.validation.ConstraintViolationException;";
		}

		if (method.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
			final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
			final String clientDomainObjName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName();

			clientParamComment = " * @param " + clientDomainObjName + clientPkAttr.getUpperCaseName() + "\n";
		}

		if (method.getDataFetchType() == BoundaryMethodDataFetchType.USER) {
			final DomainAttribute userPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.USER).getPKAttribute();
			final String userDomainObjName = project.getDomainObjectByTag(DomainTagEnumeration.USER).getLowerCaseName();

			clientParamComment = " * @param " + userDomainObjName + userPkAttr.getUpperCaseName() + "\n";
		}

		if (method.getReturnType().isPrimitive() || method.getReturnType().getNamespace() == null) {
			for (final MethodParameter p : method.getMethodParameters())
				if (p.getType() instanceof final MappingObject mappingObject) {
					domainObjectLabel = mappingObject.getDomainObject().getLabel();
					domainObjectPluralLabel = mappingObject.getDomainObject().getLabelPlural();
					break;
				}
		}
		else if (method.getReturnType().isEnum()) {
			domainObjectLabel = method.getReturnType().getName();
			domainObjectPluralLabel = domainObjectLabel + "s";
		}
		else if (method.getReturnType() instanceof final MappingObject mappingObject) {
			domainObjectLabel = mappingObject.getDomainObject().getLabel();
			domainObjectPluralLabel = mappingObject.getDomainObject().getLabelPlural();
		}
	}

	/**
	 * Replace all carriage return and line feed characters using a blank
	 * @param input
	 * @return the input string without carriage return and line feed characters
	 */
	protected String replaceCRLFCharacters(String input) {
		return input.replace("\r\n", " ").replace("\r", " ").replace("\n", " ").trim();
	}

	/**
	 * @return the default name of the repository
	 */
	public String getRepositoryName() {
		return generatorUtility.getRepositoryName();
	}

	/**
	 * @return all necessary imports
	 */
	public Set<String> getImports() {
		return generatorUtility.getImports();
	}

	/**
	 * @return the declaration of all services used locally
	 */
	public String getLocalServices() {
		return generatorUtility.getLocalServices();
	}

	/**
	 * @return the generated content
	 */
	public String createMethod() {
		final var b = new StringBuilder();
		b.append(parentGenerator.getAnnotationForGeneratedElement());

		if (project.isJakartaEEApplication() || project.isSpringBootApplication()) {
			if (method.getPermissionMode() == PermissionModeEnumeration.DENY_ALL)
				b.append("@DenyAll\n");
			else if (method.getPermissionMode() == PermissionModeEnumeration.PERMIT_ALL)
				b.append("@PermitAll\n");
			else if (!method.getRoles().isEmpty()) {
				boolean isFirstRole = true;

				for (final Role role : method.getRoles())
					if (isFirstRole) {
						isFirstRole = false;

						b.append("@RolesAllowed({\"" + role.getName() + "\"");
					}
					else
						b.append(", \"" + role.getName() + "\"");

				b.append("})\n");
			}
			else
				b.append("@DenyAll\n");

			// Add the transaction attribute
			if (project.isJakartaEEApplication()) {
				if (method.getTransactionType() != TransactionTypeEnumeration.REQUIRED)
					b.append("@TransactionAttribute(TransactionAttributeType." + method.getTransactionType() + ")\n");
			}
			else if (addTransactionAnnotation)
				b.append("@Transactional\n");
		}

		b.append(getMethodSignature(true));
		b.append("\n{\n");

		if (project.isJavaSEApplication()) {
			if (project.isBoundaryMode())
				b.append("final EntityManager em = PersistenceEngine.getEntityManager();\n");

			final String localServices = getLocalServices();

			b.append(localServices);

			if (addTransactionManagement)
				b.append("final EntityTransaction tr = em.getTransaction();\n");

			if (!localServices.isEmpty() || addTransactionManagement || project.isBoundaryMode())
				b.append("\n");

			if (addTransactionManagement || project.isBoundaryMode()) {
				b.append("try\n");
				b.append("{\n");
			}

			if (addTransactionManagement)
				b.append("tr.begin();\n\n");
		}

		b.append(createMethodBody());

		if (project.isJavaSEApplication()) {
			if (addTransactionManagement || project.isBoundaryMode())
				b.append("}\n");

			if (addTransactionManagement) {
				b.append("catch (final RuntimeException e)\n");
				b.append("{\n");
				b.append("if(tr.isActive())\n");
				b.append("tr.rollback();\n\n");
				b.append("// We just re-throw the original exception again!\n");
				b.append("throw e;\n");
				b.append("}\n");
			}

			if (project.isBoundaryMode()) {
				b.append("finally\n");
				b.append("{\n");
				b.append("PersistenceEngine.releaseEntityManager(em);\n");
				b.append("}\n");
			}
		}

		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @param addQualifier
	 * @return the method signature
	 */
	public String getMethodSignature(boolean addQualifier) {
		return generatorUtility.getMethodSignature(addQualifier, true);
	}

	/**
	 * @return all imports necessary for the service interface
	 */
	public Set<String> getInterfaceImports() {
		return generatorUtility.getInterfaceImports();
	}

	/**
	 * @return the method body
	 */
	public String createMethodBody() {
		final var b = new StringBuilder();
		var paramList = "";

		for (final MethodParameter p : method.getMethodParameters())
			paramList = paramList + p.getName() + ", ";

		if (!method.getMethodParameters().isEmpty())
			paramList = paramList.substring(0, paramList.length() - 2);

		if (!(method.getServiceMethod().getReturnType() instanceof DomainObject)) {
			if (!method.getReturnType().isVoid())
				b.append("return " + getRepositoryName() + method.getServiceMethod().getName() + "(" + paramList + ");\n");
			else
				b.append(getRepositoryName() + method.getServiceMethod().getName() + "(" + paramList + ");\n");
		}

		return b.toString();
	}

	/**
	 * @return the method's comment
	 */
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * Create the body of a list method
	 * @param filterAttribute
	 * @param pkAttr
	 * @return the generated content
	 */
	public String createListMethod(DTOBeanAttribute filterAttribute, DomainAttribute pkAttr) {
		final var b = new StringBuilder();
		final var dto = (DTOBean) method.getReturnType();
		var fromClause = method.getQueryStatement().trim() + " ";
		final String customStatement = replaceCRLFCharacters(method.getCustomStatement());
		var groupBy = "";

		if (customStatement.contains("group by")) {
			groupBy = customStatement.substring(customStatement.indexOf("group by"));
			fromClause += customStatement.substring(0, customStatement.indexOf("group by"));
		}
		else
			fromClause += customStatement;

		if (pkAttr == null && filterAttribute != null && filterAttribute.getSelectToken() != null
				&& !filterAttribute.getDomainAttribute().isWildcardFilteringSupported()) {
			b.append("if(filter != null && !filter.isEmpty() && !filter.equals(WILDCARD))\n");
			b.append("try\n");
			b.append("{\n");
			b.append(filterAttribute.getDomainAttribute().convertFromString("filter") + ";\n");
			b.append("}\n");

			if (filterAttribute.getDomainAttribute().getJavaType().isUUID())
				b.append("catch(IllegalArgumentException e)\n");
			else
				b.append("catch(NumberFormatException e)\n");

			b.append("{\n");
			b.append("return Collections.emptyList();\n");
			b.append("}\n\n");
		}

		b.append("// Collect the select tokens of all fields that should be fetched\n");
		b.append("final var selectTokens = new ArrayList<String>();\n");

		for (final DTOBeanAttribute attr : dto.getAttributes()) {
			if (attr.getSelectToken() == null)
				continue;

			b.append("selectTokens.add(" + attr.getSelectTokenConstant() + ");\n");
		}

		b.append("\n");
		b.append("// Initialize the search object\n");
		b.append("final var searchObj = new SearchDTO();\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(true);\n");
		b.append("searchObj.setMaxResult(");

		if (filterAttribute != null)
			b.append("SMALL_LIST_SIZE");
		else
			b.append("DEFAULT_LIST_SIZE");

		b.append(");\n");
		b.append("searchObj.setFromClause(\"" + fromClause.trim() + "\");\n");

		if (!groupBy.isEmpty())
			b.append("searchObj.setGroupBy(\"" + groupBy.trim() + "\");\n");

		b.append("\n");

		if (pkAttr != null && filterAttribute == null) {
			b.append("final var parentFilterField = searchObj.addSearchField(\"x." + pkAttr.getName() + "\", ");
			b.append(pkAttr.getSearchFieldDataType());
			b.append(");\n");

			// We iterate over all parameters but we do expect just one!
			method.getMethodParameters()
					.forEach(param -> b.append("parentFilterField.setFilterCriteria(" + pkAttr.convertToString(param.getName()) + ");\n"));

			b.append("\n");
		}

		if (pkAttr == null && filterAttribute != null && filterAttribute.getSelectToken() != null) {
			b.append("if(filter != null && !filter.isEmpty() && !filter.equals(WILDCARD))\n");
			b.append("{\n");
			b.append("final var filterField = searchObj.addSearchField(" + filterAttribute.getSelectTokenConstant() + ", ");
			b.append(filterAttribute.getDomainAttribute().getSearchFieldDataType());
			b.append(");\n");
			b.append("filterField.setFilterCriteria(filter");

			if (filterAttribute.getDomainAttribute().isWildcardFilteringSupported())
				b.append(" + WILDCARD");

			b.append(");\n");
			b.append("filterField.setSortIndex(1);\n");
			b.append("filterField.setSortOrder(SortDirectionEnum.ASC);\n");
			b.append("}\n\n");

			if (filterAttribute.getDomainAttribute().getDomainObject().isMandated()) {
				b.append(addAdditionalSearchField(filterAttribute));
				b.append("\n");
			}
		}

		if (pkAttr == null && filterAttribute == null && method.getDataFetchType() != BoundaryMethodDataFetchType.DEFAULT) {
			b.append(addAdditionalSearchField(null));
			b.append("\n");
		}

		b.append("return " + getRepositoryName() + REPO_METHOD_NAME_SEARCH);
		b.append("(searchObj, " + method.getReturnType().getName() + ".class, selectTokens);\n");

		return b.toString();
	}

	/**
	 * Create the finder method call
	 * @param domainObjectName
	 * @param dtoBean
	 * @param paramName
	 * @return the generated content
	 */
	public String createFindMethodCall(String domainObjectName, DTOBean dtoBean, String paramName) {
		final var b = new StringBuilder();

		// Search the finder method
		final RepositoryMethod finderMethod = method.getBoundaryBean().getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

		if (finderMethod == null)
			throw new IllegalStateException("An appropriate repository method of type 'FIND_EXISTING' could not be found!");

		final DTOBeanAttribute pkDTOAttr = dtoBean.getPKAttribute();

		if (pkDTOAttr == null)
			throw new IllegalStateException("The primary key attribute of the DTO '" + dtoBean.getName() + "' could not be found!");

		b.append("// Find and attach object\n");
		b.append(dtoBean.getDomainObject().getName() + " " + domainObjectName);
		b.append(" = " + getRepositoryName() + finderMethod.getName() + "(");
		b.append(paramName + "." + pkDTOAttr.getGetterName() + ", true);\n\n");

		return b.toString();
	}

	/**
	 * Create the getter for the given LOB attribute
	 * @param domainObject
	 * @param domainObjectName
	 * @param lobAttribute
	 * @return the generated content
	 * @throws IllegalStateException if the getter could not be created
	 */
	public String createLOBAttributeGetter(DomainObject domainObject, String domainObjectName, DomainAttribute lobAttribute) {
		for (final DomainObject o : domainObject.getFullInheritanceTree())
			if (lobAttribute.getDomainObject().equals(o))
				return domainObjectName + "." + lobAttribute.getGetterName();

		// The LOB attribute does not belong directly to this domain object!
		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
			if (assoc instanceof OneToOneAssociation) {
				final DomainObject refObject = assoc.getTarget();

				for (final DomainAttribute a : refObject.getAllAttributes())
					if (a.equals(lobAttribute)) {
						// We assume that there can be just a single one-to-one association!
						return domainObjectName + "." + assoc.getGetterName() + "." + lobAttribute.getGetterName();
					}
			}

		throw new IllegalStateException("The getter for the attribute '" + lobAttribute.getName() + "' could not be created!");
	}

	/**
	 * Create the setter for the given LOB attribute
	 * @param domainObject
	 * @param domainObjectName
	 * @param lobAttribute
	 * @return the generated content
	 * @throws IllegalStateException if the setter could not be created
	 */
	public String createLOBAttributeSetter(DomainObject domainObject, String domainObjectName, DomainAttribute lobAttribute) {
		for (final DomainObject o : domainObject.getFullInheritanceTree())
			if (lobAttribute.getDomainObject().equals(o))
				return domainObjectName + "." + lobAttribute.getSetterName();

		// The LOB attribute does not belong directly to this domain object!
		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
			if (assoc instanceof OneToOneAssociation) {
				final DomainObject refObject = assoc.getTarget();

				for (final DomainAttribute a : refObject.getAllAttributes())
					if (a.equals(lobAttribute)) {
						// We assume that there can be just a single one-to-one association!
						return domainObjectName + "." + assoc.getGetterName() + "." + lobAttribute.getSetterName();
					}
			}

		throw new IllegalStateException("The setter for the attribute '" + lobAttribute.getName() + "' could not be created!");
	}

	/**
	 * @param obj
	 * @param fragment
	 * @param assocSet
	 * @return a JPA query fragment
	 */
	private String getUserQueryFragment(DomainObject obj, String fragment, HashSet<AbstractDomainAssociation> assocSet) {
		final DomainObject objUser = obj.getNamespace().getProject().getDomainObjectByTag(DomainTagEnumeration.USER);

		if (objUser == null)
			return "";

		// First we check all associations of this domain object
		for (final AbstractDomainAssociation assoc : obj.getAllAssociations())
			if (assoc instanceof ManyToOneAssociation && assoc.getTarget().equals(objUser))
				return fragment + "." + assoc.getName() + "." + assoc.getTarget().getPKAttribute().getName();

		// Now we check the associations of the referenced domain objects
		for (final AbstractDomainAssociation assoc : obj.getAllAssociations())
			if (!obj.equals(assoc.getTarget()) && assoc instanceof final OneToOneAssociation oto) {
				// Check if this association has been already analyzed in order to avoid an infinite loop!
				if (assocSet.contains(oto))
					continue;

				assocSet.add(oto);

				if (!oto.isOptional())
					return getUserQueryFragment(oto.getTarget(), fragment + "." + assoc.getName(), assocSet);
			}

		return "";
	}

	/**
	 * @return a JPA query fragment
	 */
	private String getUserQueryFragment() {
		return getUserQueryFragment(method.getBoundaryBean().getDomainObject(), "a", new HashSet<>());
	}

	/**
	 * Add a search field to filter by either the logged on user or by the selected client
	 * @param filterAttribute
	 * @return the generated content
	 * @throws IllegalStateException if the method was not able to initialize all necessary internal fields
	 */
	public String addAdditionalSearchField(DTOBeanAttribute filterAttribute) {
		final var b = new StringBuilder();
		DomainAttribute pkAttribute = null;
		var queryFragment = "";
		var paramName = "";
		var comment = "";

		if (method.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT || filterAttribute != null) {
			pkAttribute = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
			queryFragment = CommonGenerator.getClientAccessFragment(method.getBoundaryBean().getDomainObject(), "a");
			paramName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName() + pkAttribute.getUpperCaseName();
			comment = "// Limit the number of returned objects by the selected client\n";
		}
		else if (method.getDataFetchType() == BoundaryMethodDataFetchType.USER) {
			pkAttribute = project.getDomainObjectByTag(DomainTagEnumeration.USER).getPKAttribute();
			queryFragment = getUserQueryFragment();
			paramName = project.getDomainObjectByTag(DomainTagEnumeration.USER).getLowerCaseName() + pkAttribute.getUpperCaseName();
			comment = "// Limit the number of returned objects by the logged on user\n";
		}

		if (pkAttribute == null)
			throw new IllegalStateException("The search field could not be added as the internal initialization has failed!");

		b.append(comment);
		b.append("final var clientFilterField = searchObj.addSearchField(\"" + queryFragment + "\", ");
		b.append(pkAttribute.getSearchFieldDataType());
		b.append(");\n");
		b.append("clientFilterField.setFilterCriteria(" + pkAttribute.convertToString(paramName) + ");\n");

		return b.toString();
	}

	/**
	 * @return true if the method needs the @Transactional annotation
	 */
	public boolean isAddTransactionAnnotation() {
		return addTransactionAnnotation;
	}

}

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
package net.codecadenza.eclipse.generator.repository.method;

import static net.codecadenza.eclipse.shared.Constants.PARAM_LOGGED_ON_USER;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;

/**
 * <p>
 * Base class for repository method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicRepositoryMethodGenerator {
	private static final String QUERY_PARAM_NAME_PREFIX = "PARAM_";
	private static final String EMTPY_CONTENT = "";
	private static final String EMTPY_COMMENT = "";

	private final Map<String, String> queryParameterConstants = new HashMap<>();
	protected final Project project;
	protected final RepositoryMethod method;
	protected final DomainObject domainObject;
	protected final String domainObjectLabel;
	protected final String domainObjectName;

	/**
	 * Constructor
	 * @param method
	 */
	public BasicRepositoryMethodGenerator(RepositoryMethod method) {
		this.method = method;
		this.domainObject = this.method.getRepository().getDomainObject();
		this.project = this.domainObject.getNamespace().getProject();
		this.domainObjectLabel = this.domainObject.getLabel();
		this.domainObjectName = this.domainObject.getName();
	}

	/**
	 * Create the repository method body
	 * @return the generated content
	 */
	protected String createMethodBody() {
		return EMTPY_CONTENT;
	}

	/**
	 * @return the method's comment
	 */
	protected String createComment() {
		return EMTPY_COMMENT;
	}

	/**
	 * @return a set with all required imports
	 */
	public Set<String> getImports() {
		return method.getMethodParameters().stream().filter(p -> p.getType().getNamespace() != null)
				.map(param -> "import " + param.getType().getNamespace().toString() + ".*;").collect(Collectors.toSet());
	}

	/**
	 * Create the method including the comment and all necessary annotations
	 * @return the generated content
	 */
	public String createMethod() {
		final var b = new StringBuilder();
		b.append(createComment());
		b.append(createAnnotations());
		b.append("public " + getMethodSignature());
		b.append("\n{\n");
		b.append(createMethodBody());
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return a map containing all additional repositories that are required by this method
	 */
	public Map<String, Repository> getRequiredRepositories() {
		return Collections.emptyMap();
	}

	/**
	 * @return the method signature
	 */
	public String getMethodSignature() {
		final var identifier = new StringBuilder();

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			identifier.append(method.getReturnTypeModifier() + "<" + method.getReturnType().getName() + "> ");
		else
			identifier.append(method.getReturnType().getName() + " ");

		identifier.append(method.getName() + "(");

		if (method.getMethodType() == RepositoryMethodTypeEnumeration.COUNT)
			identifier.append("SearchDTO searchObj");
		else if (method.getMethodType() == RepositoryMethodTypeEnumeration.SEARCH)
			identifier.append("String statement, int maxResults, int startIndex, Map<String, Object> addParams");
		else {
			boolean isFirstParameter = true;

			for (final MethodParameter parameter : method.getMethodParameters()) {
				if (isFirstParameter) {
					if (parameter.getModifier() != JavaTypeModifierEnumeration.NONE)
						identifier.append(parameter.getModifier() + "<" + parameter.getType().getName() + "> " + parameter.getName());
					else
						identifier.append(parameter.getType().getName() + " " + parameter.getName());

					isFirstParameter = false;
				}
				else if (parameter.getModifier() != JavaTypeModifierEnumeration.NONE)
					identifier.append(", " + parameter.getModifier() + "<" + parameter.getType().getName() + "> " + parameter.getName());
				else
					identifier.append(", " + parameter.getType().getName() + " " + parameter.getName());
			}

			if (method.getMethodType() == RepositoryMethodTypeEnumeration.PERSIST) {
				identifier.append(", boolean performChecks");
				identifier.append(", boolean performFlush");
				identifier.append(", boolean performRefresh");
			}
			else if (method.getMethodType() == RepositoryMethodTypeEnumeration.MERGE) {
				identifier.append(", boolean performChecks");
				identifier.append(", boolean performFlush");
			}
			else if (method.getMethodType() == RepositoryMethodTypeEnumeration.COPY && method.addUserParam()) {
				final DTOBean logOnDTO = project.getApplicationLogOnDTO();

				identifier
						.append(", " + logOnDTO.getPKAttribute().getDomainAttribute().getJavaType().getName() + " " + PARAM_LOGGED_ON_USER);
			}
		}

		identifier.append(")");

		return identifier.toString();
	}

	/**
	 * @return a {@link Map} with all necessary query parameter constants
	 */
	public Map<String, String> getQueryParameterConstants() {
		return queryParameterConstants;
	}

	/**
	 * Add a query parameter constant
	 * @param paramName
	 * @return the name of the query constant
	 */
	public String addQueryParameterConstant(String paramName) {
		final String paramConstant = QUERY_PARAM_NAME_PREFIX + paramName.toUpperCase();

		queryParameterConstants.put(paramConstant, paramName);

		return paramConstant;
	}

	/**
	 * Create all necessary annotations
	 * @return the generated content
	 */
	protected String createAnnotations() {
		final var b = new StringBuilder();

		if (project.isProtectManualChanges())
			b.append("@Generated\n");

		// Add security annotations if the bean runs in a managed environment
		if (!project.isBoundaryMode() && (project.isJakartaEEApplication() || project.isSpringBootApplication())) {
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
			if (project.isJakartaEEApplication() && method.getTransactionType() != TransactionTypeEnumeration.REQUIRED)
				b.append("@TransactionAttribute(TransactionAttributeType." + method.getTransactionType() + ")\n");
		}

		return b.toString();
	}

	/**
	 * @return the name of the validation exception
	 */
	protected String getValidationExceptionName() {
		if (project.getValidationType() == ValidationTypeEnumeration.STANDARD)
			return "ConstraintViolationException";

		return "PropertyConstraintViolationException";
	}

	/**
	 * Create the fragment for invoking the unique key check methods within persist and merge methods
	 * @return the generated content
	 */
	public String createUniqueKeyChecks() {
		final var b = new StringBuilder();
		final MethodParameter objectParam = method.getMethodParameters().stream().findFirst().orElse(null);
		boolean isFirstMethod = true;

		for (final RepositoryMethod checkMethod : method.getRepository().getRepositoryMethods()) {
			DomainAttribute attr1 = null;
			DomainAttribute attr2 = null;
			AbstractDomainAssociation assoc1 = null;
			AbstractDomainAssociation assoc2 = null;
			boolean addMethod = true;

			if (method.getMethodType() == RepositoryMethodTypeEnumeration.PERSIST
					&& checkMethod.getMethodType() != RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY)
				continue;

			if (method.getMethodType() == RepositoryMethodTypeEnumeration.MERGE
					&& checkMethod.getMethodType() != RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
				continue;

			// Do not call the check method within a persist operation if the check refers to a one-to-one association!
			if (method.getMethodType() == RepositoryMethodTypeEnumeration.PERSIST)
				for (final MethodParameter param : checkMethod.getMethodParameters())
					if (param instanceof final RepositoryMethodParameter repositoryParam
							&& repositoryParam.getAssociation() instanceof OneToOneAssociation) {
						addMethod = false;
						break;
					}

			if (!addMethod)
				continue;

			for (final MethodParameter param : checkMethod.getMethodParameters()) {
				if (!(param instanceof final RepositoryMethodParameter repositoryParam))
					continue;

				if (repositoryParam.getAttribute() != null) {
					final DomainAttribute pkAttr = method.getRepository().getDomainObject().getPKAttribute();

					// In the case of a 'EXISTS_BY_UNIQUE_KEY_WITH_ID' method the primary key attribute must not be used for
					// creating the exception message!
					if (pkAttr.equals(repositoryParam.getAttribute())
							&& checkMethod.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
						continue;

					if (attr1 == null)
						attr1 = repositoryParam.getAttribute();
					else
						attr2 = repositoryParam.getAttribute();
				}
				else if (assoc1 == null)
					assoc1 = repositoryParam.getAssociation();
				else
					assoc2 = repositoryParam.getAssociation();
			}

			if (isFirstMethod) {
				isFirstMethod = false;

				b.append("// Perform unique key checks\n");
			}
			else
				b.append("\n");

			b.append("if(performChecks && " + checkMethod.getName() + "(");

			boolean isFirstParam = true;

			for (final MethodParameter param : checkMethod.getMethodParameters()) {
				if (isFirstParam)
					isFirstParam = false;
				else
					b.append(", ");

				if (param instanceof final RepositoryMethodParameter repositoryParam) {
					if (repositoryParam.getAttribute() != null)
						b.append(objectParam.getName() + "." + repositoryParam.getAttribute().getGetterName());
					else if (repositoryParam.getAssociation() != null)
						b.append(getPrimaryKeyAttributeValue(objectParam.getName(), repositoryParam.getAssociation()));
				}
				else {
					// If the method parameter is not a repository method parameter it will represent the primary key attribute of the
					// domain object to be checked!
					final var attributeGetter = "get" + param.getName().substring(0, 1).toUpperCase() + param.getName().substring(1);

					b.append(objectParam.getName() + "." + attributeGetter + "()");
				}
			}

			b.append("))\n");
			b.append(createThrowUniqueKeyException(attr1, attr2, assoc1, assoc2));
		}

		if (!isFirstMethod)
			b.append("\n");

		return b.toString();
	}

	/**
	 * Create the statement that throws a unique key violation exception based on the provided attributes and associations
	 * @param attr1
	 * @param attr2
	 * @param assoc1
	 * @param assoc2
	 * @return the generated content
	 */
	private String createThrowUniqueKeyException(DomainAttribute attr1, DomainAttribute attr2, AbstractDomainAssociation assoc1,
			AbstractDomainAssociation assoc2) {
		final var b = new StringBuilder();
		final MethodParameter objectParam = method.getMethodParameters().stream().findFirst().orElseThrow();

		b.append("throw new UniqueConstraintViolationException(\"");
		b.append(domainObjectLabel.substring(0, 1).toUpperCase() + domainObjectLabel.substring(1));

		if (attr1 != null) {
			b.append(" with ");
			b.append(attr1.getLabel());
			b.append(" '\" + ");
			b.append(objectParam.getName() + "." + attr1.getGetterName());

			if (attr2 != null) {
				b.append("+ \"'");
				b.append(" and ");
				b.append(attr2.getLabel());
				b.append(" '\" + ");
				b.append(objectParam.getName() + "." + attr2.getGetterName());
			}
		}

		if (assoc1 != null) {
			if (attr1 != null) {
				b.append("+ \"'");
				b.append(" and ");
			}
			else
				b.append(" with ");

			b.append(assoc1.getTarget().getLabel());
			b.append(" '\" + ");
			b.append(getPrimaryKeyAttributeValue(objectParam.getName(), assoc1));

			if (assoc2 != null) {
				b.append(" + \"'");
				b.append(" and ");
				b.append(assoc2.getTarget().getLabel());
				b.append(" '\" + ");
				b.append(getPrimaryKeyAttributeValue(objectParam.getName(), assoc2));
			}
		}

		if (assoc1 == null && attr1 == null)
			b.append(" already exists!\");\n");
		else
			b.append(" + \"' already exists!\");\n");

		return b.toString();
	}

	/**
	 * Create a null-safe getter for the primary key attribute of the given association
	 * @param paramName the name of the parameter
	 * @param assoc the domain association
	 * @return the generated fragment
	 */
	protected String getPrimaryKeyAttributeValue(String paramName, AbstractDomainAssociation assoc) {
		final var b = new StringBuilder();
		final boolean isOptional = (assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
				|| (assoc instanceof final OneToOneAssociation oto && oto.isOptional());

		if (isOptional)
			b.append("(");

		b.append(paramName + "." + assoc.getGetterName());

		if (isOptional) {
			// The parameter that is mapped to an optional association has never a primitive type!
			b.append(" != null ? ");
			b.append(paramName + "." + assoc.getGetterName());
			b.append("." + assoc.getTarget().getPKAttribute().getGetterName());
			b.append(" : ");
			b.append("null");
		}
		else
			b.append("." + assoc.getTarget().getPKAttribute().getGetterName());

		if (isOptional)
			b.append(")");

		return b.toString();
	}

}

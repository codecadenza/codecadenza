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
package net.codecadenza.eclipse.generator.repository.method.imp;

import java.util.Set;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for unique key repository methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UniqueKeyRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	private final RepositoryMethodTypeEnumeration methodType;

	/**
	 * Constructor
	 * @param method
	 */
	public UniqueKeyRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);

		this.methodType = method.getMethodType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();
		imports.add("import jakarta.persistence.*;");

		if (methodType == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY
				|| methodType == RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)
			imports.add("import java.util.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createComment()
	 */
	@Override
	protected String createComment() {
		final var b = new StringBuilder();
		final String comment;

		if (methodType == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY)
			comment = "Find a persistent " + domainObjectLabel + " object by using the provided parameters";
		else if (methodType == RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)
			comment = "Search for " + domainObjectLabel + " objects by using the provided parameters";
		else
			comment = "Check if the given " + domainObjectLabel + " already exists";

		b.append("/**\n");
		b.append(" * " + comment + "\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		if (methodType == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY) {
			b.append(" * @return the " + domainObjectLabel + " object or null if it could not be found\n");
			b.append(" * @throws IllegalStateException if the query returned more than one object\n");
		}
		else if (methodType == RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)
			b.append(" * @return a list that contains all " + domainObjectLabel + " objects that match the provided filter criteria\n");
		else
			b.append(" * @return true if the " + domainObjectLabel + " already exists\n");

		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final var queryParams = new StringBuilder();
		final var paramCheck = new StringBuilder();
		final var queryStatement = new StringBuilder();
		String pkParamName = null;
		boolean isFirstParam = true;
		boolean hasOptionalParams = false;

		for (final MethodParameter param : method.getMethodParameters()) {
			if (isFirstParam && method.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID) {
				isFirstParam = false;
				pkParamName = param.getName();

				if (!param.getType().isPrimitive())
					paramCheck.append(addParamCheck(pkParamName));

				continue;
			}

			final var repositoryParam = (RepositoryMethodParameter) param;
			final String paramName = param.getName();
			String queryParamName = param.getName();
			boolean isOptional = false;
			var paramPath = "";
			var paramOptPath = "";

			if (repositoryParam.getAttribute() != null) {
				paramPath = repositoryParam.getAttribute().getName();
				paramOptPath = repositoryParam.getAttribute().getName();
				isOptional = repositoryParam.getAttribute().getDomainAttributeValidator().isNullable();

				if (!repositoryParam.getAttribute().getJavaType().isPrimitive() && !isOptional)
					b.append(addParamCheck(paramName));
			}
			else {
				final AbstractDomainAssociation assoc = repositoryParam.getAssociation();

				paramPath = assoc.getName() + "." + assoc.getTarget().getPKAttribute().getName();
				paramOptPath = assoc.getName();

				isOptional = (assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
						|| (assoc instanceof final OneToOneAssociation oto && oto.isOptional());

				// Make sure to use the correct name defined in the named query!
				queryParamName = assoc.getName();

				if (!repositoryParam.getAssociation().getTarget().getPKAttribute().getJavaType().isPrimitive() && !isOptional)
					b.append(addParamCheck(paramName));
			}

			final var predicate = isFirstParam ? " where" : " and";

			if (isOptional) {
				queryParams.append("\nif(" + paramName + " != null)\n");

				queryStatement.append("\nif(" + paramName + " == null)\n");
				queryStatement.append("queryStatement += \"" + predicate + " a." + paramOptPath + " is null\";\n");
				queryStatement.append("else\n");
			}

			queryStatement.append("queryStatement += \"" + predicate + " a." + paramPath + " = :" + queryParamName + "\";\n");
			queryParams.append("query.setParameter(" + addQueryParameterConstant(queryParamName) + ", " + paramName + ");\n");

			if (isOptional) {
				hasOptionalParams = true;
				queryStatement.append("\n");
				queryParams.append("\n");
			}

			isFirstParam = false;
		}

		b.append(paramCheck);

		if (methodType == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY
				|| methodType == RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)
			b.append(createFindQuery(hasOptionalParams, queryStatement.toString(), queryParams.toString()));
		else
			b.append(createExistsQuery(hasOptionalParams, pkParamName, queryStatement.toString(), queryParams.toString()));

		return b.toString();
	}

	/**
	 * Create and initialize the query for checking if an object exists
	 * @param createCustomStatement if true a custom query needs to be generated
	 * @param pkParamName the name of the parameter that represents the primary key attribute
	 * @param customFilterStatements a list of additional statements if the default named query cannot be used
	 * @param queryParams a list of statements for setting the query parameters
	 * @return the generated content
	 */
	private String createExistsQuery(boolean createCustomStatement, String pkParamName, String customFilterStatements,
			String queryParams) {
		final var b = new StringBuilder();
		final String pkAttributeName = domainObject.getPKAttribute().getName();

		if (createCustomStatement) {
			// If at least one parameter is optional the respective named query cannot be used!
			b.append("var queryStatement = \"select count(a) from " + domainObjectName + " a\";\n");

			if (pkParamName != null)
				b.append("queryStatement += \" where a." + pkAttributeName + " <> :" + pkParamName + "\";\n");

			b.append(customFilterStatements);
			b.append("\n");
			b.append("final TypedQuery<Long> query = em.createQuery(queryStatement, Long.class);\n");
		}
		else {
			b.append("final TypedQuery<Long> query = em.createNamedQuery(");
			b.append(domainObjectName + "." + method.getHint() + ", Long.class);\n");
		}

		if (pkParamName != null) {
			// When performing an update operation the entity manager should not sync its state with the database before this query has
			// been executed! Otherwise the query makes no sense as the SQL update statement already causes an exception regarding
			// unique key constraint violations!
			b.append("query.setFlushMode(FlushModeType.COMMIT);\n");
			b.append("query.setParameter(" + addQueryParameterConstant(pkParamName) + ", " + pkParamName + ");\n");
		}

		b.append(queryParams);
		b.append("\nreturn query.getSingleResult() != 0;\n");

		return b.toString();
	}

	/**
	 * Create and initialize the query for finding objects
	 * @param createCustomStatement if true a custom query needs to be generated
	 * @param customFilterStatements a list of additional statements if the default named query cannot be used
	 * @param queryParams a list of statements for setting the query parameters
	 * @return the generated content
	 */
	private String createFindQuery(boolean createCustomStatement, String customFilterStatements, String queryParams) {
		final var b = new StringBuilder();

		if (createCustomStatement) {
			b.append("var queryStatement = \"select a from " + domainObjectName + " a\";\n");
			b.append(customFilterStatements);
			b.append("\n");
			b.append("final TypedQuery<" + domainObjectName + "> query = ");
			b.append("em.createQuery(queryStatement, " + domainObjectName + ".class);\n");
		}
		else {
			b.append("final TypedQuery<" + domainObjectName + "> query = em.createNamedQuery(");
			b.append(domainObjectName + "." + method.getHint() + ", " + domainObjectName + ".class);\n");
		}

		b.append(queryParams);

		if (methodType == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY) {
			b.append("\nfinal List<" + domainObjectName + "> resultList = query.getResultList();\n\n");
			b.append("if(resultList.size() <= 1)\n");
			b.append("return resultList.stream().findFirst().orElse(null);\n\n");
			b.append("throw new IllegalStateException(\"Non unique result!\");\n");
		}
		else
			b.append("\nreturn query.getResultList();\n");

		return b.toString();
	}

	/**
	 * Add a non-null check for the given parameter
	 * @param paramName
	 * @return the generated content
	 */
	private String addParamCheck(final String paramName) {
		final var b = new StringBuilder();
		b.append("if(" + paramName + " == null)\n");
		b.append("throw new IllegalArgumentException(\"Parameter \\\"");
		b.append(paramName + "\\\" must not be null!\");\n\n");

		return b.toString();
	}

}

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
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#EXISTS_BY_UNIQUE_KEY_WITH_ID} and
 * {@link RepositoryMethodTypeEnumeration#EXISTS_BY_UNIQUE_KEY}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExistsByUniqueKeyRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 */
	public ExistsByUniqueKeyRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();
		imports.add("import jakarta.persistence.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createComment()
	 */
	@Override
	protected String createComment() {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Check if the given " + domainObjectLabel + " already exists\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

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
		final String pkAttributeName = domainObject.getPKAttribute().getName();
		String pkParamName = null;
		boolean isFirstParam = true;
		boolean hasOptionalParams = false;

		for (final MethodParameter param : method.getMethodParameters()) {
			// The first parameter should not be considered!
			if (isFirstParam && method.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID) {
				isFirstParam = false;
				pkParamName = param.getName();

				if (!param.getType().isPrimitive()) {
					paramCheck.append("if(" + pkParamName + " == null)\n");
					paramCheck.append("throw new IllegalArgumentException(\"Parameter \\\"");
					paramCheck.append(pkParamName + "\\\" must not be null!\");\n\n");
				}

				continue;
			}

			final var repositoryParam = (RepositoryMethodParameter) param;
			boolean isOptional = false;
			var paramName = "";
			var paramPath = "";
			var paramGetter = "";
			var paramOptPath = "";

			if (repositoryParam.getAttribute() != null) {
				paramName = repositoryParam.getAttribute().getName();
				paramPath = repositoryParam.getName();
				paramOptPath = repositoryParam.getName();
				paramGetter = repositoryParam.getName();
				isOptional = repositoryParam.getAttribute().getDomainAttributeValidator().isNullable();

				if (isOptional)
					hasOptionalParams = true;

				if (!repositoryParam.getAttribute().getJavaType().isPrimitive() && !isOptional) {
					paramCheck.append("if(" + repositoryParam.getName() + " == null)\n");
					paramCheck.append("throw new IllegalArgumentException(\"Parameter \\\"");
					paramCheck.append(repositoryParam.getName() + "\\\" must not be null!\");\n\n");
				}
			}
			else {
				paramName = repositoryParam.getAssociation().getName();
				paramPath = repositoryParam.getAssociation().getName() + "."
						+ repositoryParam.getAssociation().getTarget().getPKAttribute().getName();
				paramGetter = repositoryParam.getAssociation().getName() + "."
						+ repositoryParam.getAssociation().getTarget().getPKAttribute().getGetterName();
				paramOptPath = repositoryParam.getAssociation().getName();

				if (repositoryParam.getAssociation() instanceof final ManyToOneAssociation mto)
					isOptional = mto.isOptional();
				else if (repositoryParam.getAssociation() instanceof final OneToOneAssociation oto)
					isOptional = oto.isOptional();

				if (!isOptional) {
					paramCheck.append("if(" + repositoryParam.getName() + " == null)\n");
					paramCheck.append("throw new IllegalArgumentException(\"Parameter \\\"");
					paramCheck.append(repositoryParam.getName() + "\\\" must not be null!\");\n\n");
				}
				else
					hasOptionalParams = true;
			}

			final var concat = isFirstParam ? "where" : "and";

			if (isOptional) {
				queryParams.append("\nif(" + repositoryParam.getName() + " != null)\n");
				queryStatement.append("\nif(" + repositoryParam.getName() + " == null)\n");
				queryStatement.append("queryStatement += \"" + concat + " a." + paramOptPath + " is null \";\n");
				queryStatement.append("else\n");
			}

			queryStatement.append("queryStatement += \"" + concat + " a." + paramPath + " = :" + paramName + " \";\n");
			queryParams.append("query.setParameter(" + addQueryParameterConstant(paramName) + ", " + paramGetter + ");\n");

			if (isOptional) {
				queryStatement.append("\n");
				queryParams.append("\n");
			}

			isFirstParam = false;
		}

		b.append(paramCheck);

		if (hasOptionalParams) {
			// If at least one parameter is optional the respective named query cannot be used!
			b.append("var queryStatement = \"select count(a) from " + domainObjectName + " a \";\n");

			if (pkParamName != null)
				b.append("queryStatement += \"where a." + pkAttributeName + " <> :" + pkParamName + " \";\n");

			b.append(queryStatement);
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

}

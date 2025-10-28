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

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;

import java.util.Set;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#CHANGE_PARENT}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ChangeParentRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	private final RepositoryMethod checkMethod;

	/**
	 * Constructor
	 * @param method
	 */
	public ChangeParentRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);

		this.checkMethod = getCheckMethod();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();

		if (checkMethod != null)
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createComment()
	 */
	@Override
	protected String createComment() {
		final var b = new StringBuilder();
		final String label = method.getMethodParameters().stream().skip(1).map(MethodParameter::getName).findFirst().orElse("");

		b.append("/**\n");
		b.append(" * Change the '" + label + "' attribute of this " + domainObjectLabel + "\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		if (checkMethod != null)
			b.append(" * @throws UniqueConstraintViolationException if the unique constraint check has failed\n");

		b.append(" * @throws " + getValidationExceptionName() + " if the validation of the persistent attributes has failed\n");
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
		final DomainAttribute pkAttribute = domainObject.getPKAttribute();

		b.append("final " + domainObjectName + " entity = " + REPO_METHOD_NAME_FIND_BY_ID + "(");
		b.append(pkAttribute.getName() + ", true);\n\n");

		if (checkMethod != null)
			b.append(createUniqueKeyCheck());

		boolean isFirstParam = true;

		for (final MethodParameter param : method.getMethodParameters()) {
			if (isFirstParam) {
				isFirstParam = false;
				continue;
			}

			if (param instanceof final RepositoryMethodParameter repositoryParam) {
				if (repositoryParam.getAssociation() != null)
					b.append("entity." + repositoryParam.getAssociation().getSetterName());
				else
					b.append("entity.set" + param.getName().substring(0, 1).toUpperCase() + param.getName().substring(1));

				b.append("(" + param.getName() + ");\n");
			}
		}

		return b.toString();
	}

	/**
	 * Create the unique key check fragment
	 * @return the generated content
	 */
	private String createUniqueKeyCheck() {
		final var b = new StringBuilder();
		final RepositoryMethodParameter parentParam = getParentParameter(checkMethod);

		b.append("// Perform unique key check\n");
		b.append("if(" + checkMethod.getName());
		b.append("(");

		boolean isFirstParam = true;

		for (final MethodParameter param : checkMethod.getMethodParameters()) {
			if (isFirstParam)
				isFirstParam = false;
			else
				b.append(", ");

			if (param instanceof final RepositoryMethodParameter repositoryParam) {
				if (repositoryParam.getAttribute() != null)
					b.append("entity." + repositoryParam.getAttribute().getGetterName());
				else if (repositoryParam.getAssociation() != null) {
					final AbstractDomainAssociation assoc = repositoryParam.getAssociation();

					if (repositoryParam.equals(parentParam)) {
						final String paramName = method.getMethodParameters().getLast().getName();
						final String pkAttributeGetter = assoc.getTarget().getPKAttribute().getGetterName();
						final boolean isOptional = (assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
								|| (assoc instanceof final OneToOneAssociation oto && oto.isOptional());

						b.append(paramName);

						if (isOptional)
							b.append(" != null ? " + paramName + "." + pkAttributeGetter + " :  null");
						else
							b.append("." + pkAttributeGetter);
					}
					else
						b.append(getPrimaryKeyAttributeValue("entity", assoc));
				}
			}
			else
				b.append(method.getFirstParameter().getName());
		}

		b.append("))\n");
		b.append("throw new UniqueConstraintViolationException(\"A unique key constraint check for ");
		b.append(domainObjectLabel + " with ID '\" + " + method.getFirstParameter().getName() + " + \"' has failed!\");\n\n");

		return b.toString();
	}

	/**
	 * Search for an appropriate repository method of type EXISTS_BY_UNIQUE_KEY_WITH_ID
	 * @return the unique key check method or null if no suitable method could be found
	 */
	private RepositoryMethod getCheckMethod() {
		for (final RepositoryMethod repositoryMethod : method.getRepository().getRepositoryMethods()) {
			if (repositoryMethod.getMethodType() != RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
				continue;

			if (getParentParameter(repositoryMethod) != null)
				return repositoryMethod;
		}

		return null;
	}

	/**
	 * Get the parent parameter
	 * @param checkMethod
	 * @return the parent parameter or null if it could not be found
	 */
	private RepositoryMethodParameter getParentParameter(RepositoryMethod checkMethod) {
		for (final MethodParameter param : checkMethod.getMethodParameters())
			if (param instanceof final RepositoryMethodParameter repositoryParam1 && repositoryParam1.getAssociation() != null)
				for (final MethodParameter paramMethod : method.getMethodParameters())
					if (paramMethod instanceof final RepositoryMethodParameter repositoryParam2 && repositoryParam2.getAssociation() != null
							&& repositoryParam2.getAssociation().equals(repositoryParam1.getAssociation()))
						return repositoryParam1;

		return null;
	}

}

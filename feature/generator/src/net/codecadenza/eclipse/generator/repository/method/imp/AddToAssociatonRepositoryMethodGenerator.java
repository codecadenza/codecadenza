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
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#ADD_TO_ASSOCIATION}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AddToAssociatonRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 */
	public AddToAssociatonRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();
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
		final JavaType paramType = method.getMethodParameters().stream().skip(1).map(MethodParameter::getType).findFirst()
				.orElse(null);
		final var refBeanLabel = paramType != null ? ((DomainObject) paramType).getLabel() : "";

		b.append("/**\n");
		b.append(" * Add the persistent " + refBeanLabel + " object to the corresponding list of this " + domainObjectLabel + "\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		b.append(" * @throws DuplicateCollectionEntryException if the caller tries to add an element to the collection twice\n");
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
		boolean isFirstParam = true;

		b.append("final " + domainObjectName + " bean = " + REPO_METHOD_NAME_FIND_BY_ID + "(");
		b.append(pkAttribute.getName() + ", true);\n\n");

		for (final MethodParameter param : method.getMethodParameters()) {
			if (isFirstParam) {
				isFirstParam = false;
				continue;
			}

			String assocGetter = null;
			DomainAttribute refPkAttr = null;

			// Search for the primary key of the association target
			for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
				if (assoc.getName().equals(param.getHint())) {
					refPkAttr = assoc.getTarget().getPKAttribute();
					assocGetter = assoc.getGetterName();
					break;
				}

			if (refPkAttr != null) {
				b.append("// Prevent duplicate entries\n");
				b.append("for(final " + param.getType().getName() + " item : bean." + assocGetter + ")\n");

				if (refPkAttr.getJavaType().isPrimitive()) {
					b.append("if(" + param.getName() + "." + refPkAttr.getGetterName() + " == item.");
					b.append(refPkAttr.getGetterName() + ")\n");
				}
				else {
					b.append("if(" + param.getName() + "." + refPkAttr.getGetterName() + ".equals(item.");
					b.append(refPkAttr.getGetterName() + "))\n");
				}

				b.append("throw new DuplicateCollectionEntryException(\"Entry already exists in this collection!\");\n\n");
				b.append("bean." + assocGetter + ".add(" + param.getName() + ");\n");
			}
		}

		return b.toString();
	}

}

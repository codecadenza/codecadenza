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
package net.codecadenza.eclipse.generator.domain;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.shared.Constants;

/**
 * <p>
 * Generator for the domain object meta-model
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainMetaModelGenerator extends AbstractJavaSourceGenerator {
	private final DomainObject domainObject;
	private final String packageName;

	/**
	 * Constructor
	 * @param domainObject
	 */
	public DomainMetaModelGenerator(DomainObject domainObject) {
		super(domainObject.getMetaModelSourceFile());

		this.domainObject = domainObject;
		this.packageName = domainObject.getNamespace().toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.persistence.metamodel");

		if (domainObject.getParent() != null) {
			final String rootDomainObjectPackage = domainObject.getParent().getNamespace().toString();

			if (!rootDomainObjectPackage.equals(packageName))
				importPackage(rootDomainObjectPackage);
		}

		domainObject.getAssociations().stream().map(assoc -> assoc.getTarget().getNamespace().toString()).forEach(refPackage -> {
			if (!refPackage.equals(packageName))
				importPackage(refPackage);
		});

		for (final DomainAttribute attr : domainObject.getAttributes()) {
			if (attr.getCollectionType() != CollectionTypeEnumeration.NONE)
				importPackage(Constants.PACK_JAVA_UTIL);

			if (attr.getJavaType().getNamespace() == null)
				continue;

			final String attrPackage = attr.getJavaType().getNamespace().toString();

			if (!attrPackage.equals(packageName))
				importPackage(attrPackage);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@StaticMetamodel(" + domainObject.getName() + ".class)\n");
		b.append("public class " + domainObject.getName() + "_");

		if (domainObject.getParent() != null)
			b.append(" extends " + domainObject.getParent().getName() + "_");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		domainObject.getAttributes().forEach(attr -> {
			final String attrTypeName;

			if (attr.getCollectionType() == CollectionTypeEnumeration.NONE)
				attrTypeName = attr.getJavaType().getWrapperTypeName();
			else
				attrTypeName = attr.getTypeName();

			addPublicField("SingularAttribute<" + domainObject.getName() + ", " + attrTypeName + ">", attr.getName())
					.withStaticModifier().withVolatileModifier().create();
		});

		domainObject.getAssociations().forEach(assoc -> {
			var typeName = "SingularAttribute";

			if (assoc instanceof OneToManyAssociation || assoc instanceof ManyToManyAssociation)
				typeName = "CollectionAttribute";

			addPublicField(typeName + "<" + domainObject.getName() + ", " + assoc.getTarget().getName() + ">", assoc.getName())
					.withStaticModifier().withVolatileModifier().create();
		});
	}

}

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

import static net.codecadenza.eclipse.shared.Constants.PARAM_LOGGED_ON_USER;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_GET_REFERENCE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_PERSIST;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#COPY}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CopyRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	private static final String SOURCE_OBJ_NAME = "sourceObject";
	private static final String TARGET_OBJ_NAME = "targetObject";

	private final AbstractJavaSourceGenerator parentGenerator;
	private final Map<String, Repository> requiredRepositories = new HashMap<>();
	private final boolean flushAndRefresh;

	/**
	 * Constructor
	 * @param method
	 * @param parentGenerator
	 */
	public CopyRepositoryMethodGenerator(RepositoryMethod method, AbstractJavaSourceGenerator parentGenerator) {
		super(method);

		this.parentGenerator = parentGenerator;
		this.flushAndRefresh = domainObject.getPKAttribute().getDomainObject().getIDGenerator()
				.getGeneratorType() != IDGeneratorTypeEnumeration.NONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		if (method.addUserParam()
				&& project.getApplicationLogOnDTO().getPKAttribute().getDomainAttribute().getJavaType().getNamespace() != null)
			imports.add("import "
					+ project.getApplicationLogOnDTO().getPKAttribute().getDomainAttribute().getJavaType().getNamespace().toString()
					+ ".*;");

		if (!flushAndRefresh) {
			if (domainObject.getPKAttribute().getJavaType().isString())
				imports.add("import net.codecadenza.runtime.random.*;");
			else if (domainObject.getPKAttribute().getJavaType().isUUID())
				imports.add("import java.util.*;");
			else
				imports.add("import java.util.concurrent.*;");
		}

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
		b.append(" * Create a deep copy of the given " + domainObjectLabel + "\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		if (method.addUserParam())
			b.append(" * @param " + PARAM_LOGGED_ON_USER + "\n");

		if (method.addUniqueCheck())
			b.append(" * @throws UniqueConstraintViolationException if a unique constraint check has failed\n");

		b.append(" * @throws " + getValidationExceptionName() + " if the validation of the persistent attributes has failed\n");
		b.append(" * @return the new " + domainObjectLabel + "\n");
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
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final RepositoryMethod persistMethod = method.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.PERSIST);

		// If the domain object has a primary key that is created by the database we must call flush() at the end of the method in
		// order to get the generated primary key value!
		if (flushAndRefresh)
			b.append("boolean flushAndRefresh = false;\n\n");

		b.append("if(" + TARGET_OBJ_NAME + " == null)\n");
		b.append("{\n");

		if (flushAndRefresh)
			b.append("flushAndRefresh = true;\n\n");

		b.append(TARGET_OBJ_NAME + " = new " + domainObjectName + "();\n");

		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
			if (assoc.isOwner())
				continue;

			if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
				b.append(TARGET_OBJ_NAME + "." + assoc.getSetterName() + "(");
				b.append(SOURCE_OBJ_NAME + "." + assoc.getGetterName() + ");\n");
			}
		}

		b.append("}\n\n");

		for (final DomainAttribute attr : domainObject.getAllAttributes()) {
			if (attr.isSetDateOnPersist() || attr.isSetDateOnUpdate() || attr.isTrackVersion())
				continue;

			if (attr.isPk()) {
				if (!flushAndRefresh) {
					// The primary key attribute must be initialized with a proper default value!
					b.append(TARGET_OBJ_NAME + "." + attr.getSetterName() + "(");

					if (attr.getJavaType().isString()) {
						final Integer maxLength = attr.getDomainAttributeValidator().getMaxLength();
						final int length = maxLength != null ? maxLength : 10;

						b.append("RandomStringGenerator.generateRandomString(" + length + ")");
					}
					else if (attr.getJavaType().isUUID())
						b.append("UUID.randomUUID()");
					else if (attr.getJavaType().isLong())
						b.append("Math.absExact(ThreadLocalRandom.current().nextLong())");
					else
						b.append("Math.absExact(ThreadLocalRandom.current().nextInt())");

					b.append(");\n");
				}

				continue;
			}

			if (attr.getCollectionType() == CollectionTypeEnumeration.NONE
					|| attr.getCollectionMappingStrategy() == CollectionMappingStrategyEnumeration.CONVERTER) {
				b.append(TARGET_OBJ_NAME + "." + attr.getSetterName() + "(");

				if (attr.isDisplayAttribute()) {
					b.append("\"N/A\"");
					b.append(");\n");
					continue;
				}

				b.append(SOURCE_OBJ_NAME + "." + attr.getGetterName() + ");\n");
			}
			else {
				// Copy every item one by one as the persistence provider is tracking the owner which must be a single entity
				b.append("\n");
				b.append("for(final var item : ");
				b.append(SOURCE_OBJ_NAME + "." + attr.getGetterName() + ")\n");
				b.append(TARGET_OBJ_NAME + "." + attr.getGetterName() + ".add(item);\n\n");
			}
		}

		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
			if (!assoc.isOwner())
				continue;

			if (assoc instanceof ManyToOneAssociation && method.addUserParam()
					&& assoc.getTarget().equals(logOnDTO.getDomainObject())) {
				parentGenerator.importPackage(logOnDTO.getDomainObject().getNamespace().toString());

				b.append(TARGET_OBJ_NAME + "." + assoc.getSetterName() + "(" + REPO_METHOD_NAME_GET_REFERENCE + "(");
				b.append(logOnDTO.getDomainObject().getName() + ".class, " + PARAM_LOGGED_ON_USER + "));\n");
				continue;
			}

			if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
				b.append(TARGET_OBJ_NAME + "." + assoc.getSetterName() + "(");
				b.append(SOURCE_OBJ_NAME + "." + assoc.getGetterName() + ");\n");
			}
		}

		b.append("\n");

		if (persistMethod != null) {
			b.append(TARGET_OBJ_NAME + " = ");
			b.append(persistMethod.getName() + "(" + TARGET_OBJ_NAME + (persistMethod.addUniqueCheck() ? ", true" : ""));
			b.append(", false, false);\n");
		}
		else
			b.append(REPO_METHOD_NAME_PERSIST + "(" + TARGET_OBJ_NAME + ");\n");

		// Test if the domain object references a physical file
		if (domainObject.getTag() == DomainTagEnumeration.DOCUMENT)
			for (final DomainAttribute attr : domainObject.getAllAttributes()) {
				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_REF) {
					parentGenerator.importPackage("net.codecadenza.runtime.file");
					parentGenerator.importPackage("java.io");

					b.append("\n// Create a copy of the physical file that belongs to the source object\n");
					b.append("try\n");
					b.append("{\n");
					b.append("final String targetPath = FileUtil.getUniqueFileName(" + domainObjectName + ".class.getSimpleName());\n\n");
					b.append("FileUtil.copyFile(new File(" + SOURCE_OBJ_NAME + "." + attr.getGetterName() + "), new File(targetPath));\n");
					b.append(TARGET_OBJ_NAME + "." + attr.getSetterName() + "(targetPath);\n");
					b.append("}\n");
					b.append("catch (final Exception e)\n");
					b.append("{\n");
					b.append("throw new FileOperationException(e);\n");
					b.append("}\n\n");
					break;
				}
			}

		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
			final DomainObject targetDomainObject = assoc.getTarget();
			boolean isOneToMany = false;
			boolean isManyToMany = false;
			var repositoryName = "";
			RepositoryMethod copyMethod = null;
			AbstractDomainAssociation reverseAssoc = null;

			if (!assoc.isOwner() || assoc.getTarget().isAbstract())
				continue;

			if (assoc instanceof final OneToManyAssociation otm) {
				reverseAssoc = otm.getReverseAssociation();
				isOneToMany = true;

				// Skip the deep copy of the referenced target objects of a unidirectional one-to-many association!
				if (reverseAssoc == null)
					continue;
			}
			else if (assoc instanceof final OneToOneAssociation oto) {
				reverseAssoc = oto.getReverseAssociation();

				if (reverseAssoc == null)
					continue;
			}
			else if (assoc instanceof ManyToManyAssociation)
				isManyToMany = true;
			else
				continue;

			if (targetDomainObject.equals(domainObject))
				copyMethod = method.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.COPY);
			else {
				for (final Repository repository : project.getAllRepositoriesOfProject()) {
					if (!repository.getDomainObject().equals(assoc.getTarget()))
						continue;

					if (!project.isBoundaryMode()) {
						final BoundaryBean boundary = project.getBoundaryByDomainObject(assoc.getTarget());

						if (boundary != null) {
							repositoryName = boundary.getInterfaceName();
							repositoryName = repositoryName.substring(0, 1).toLowerCase() + repositoryName.substring(1);
						}
					}
					else
						repositoryName = assoc.getTarget().getLowerCaseName() + "Manager";

					// If the project uses the facade mode it could be the case that a respective boundary doesn't exist!
					if (!repositoryName.isEmpty()) {
						// An additional repository is not necessary for many-to-many associations!
						if (!isManyToMany)
							requiredRepositories.put(repositoryName, repository);

						repositoryName += ".";
						copyMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.COPY);
					}
				}
			}

			if (copyMethod == null)
				continue;

			parentGenerator.importPackage(assoc.getTarget().getNamespace().toString());

			b.append("\n");

			if (isOneToMany) {
				final var itemName = "new" + assoc.getTarget().getName();

				b.append("for(final " + assoc.getTarget().getName() + " " + assoc.getTarget().getLowerCaseName());
				b.append(" : " + SOURCE_OBJ_NAME + "." + assoc.getGetterName() + ")\n");
				b.append("{\n");
				b.append("var " + itemName + " = new " + assoc.getTarget().getName() + "();\n");
				b.append(itemName + "." + reverseAssoc.getSetterName() + "(" + TARGET_OBJ_NAME + ");\n\n");
				b.append(itemName + " = " + repositoryName + copyMethod.getName() + "(");
				b.append(assoc.getTarget().getLowerCaseName() + ", " + itemName);

				if (method.addUserParam())
					b.append(", " + PARAM_LOGGED_ON_USER);

				b.append(");\n");
				b.append(TARGET_OBJ_NAME + "." + assoc.getGetterName() + ".add(" + itemName + ");\n");
				b.append("}\n");
			}
			else if (isManyToMany) {
				b.append("for(final " + assoc.getTarget().getName() + " " + assoc.getTarget().getLowerCaseName());
				b.append(" : " + SOURCE_OBJ_NAME + "." + assoc.getGetterName() + ")\n");
				b.append(TARGET_OBJ_NAME + "." + assoc.getGetterName() + ".add(" + assoc.getTarget().getLowerCaseName() + ");\n");
			}
			else {
				b.append("var " + assoc.getName() + " = new " + assoc.getTarget().getName() + "();\n");
				b.append(assoc.getName() + "." + reverseAssoc.getSetterName() + "(" + TARGET_OBJ_NAME + ");\n\n");
				b.append(assoc.getName() + " = " + repositoryName + copyMethod.getName() + "(" + SOURCE_OBJ_NAME + ".");
				b.append(assoc.getGetterName() + ", " + assoc.getName());

				if (method.addUserParam())
					b.append(", " + PARAM_LOGGED_ON_USER);

				b.append(");\n");
				b.append(TARGET_OBJ_NAME + "." + assoc.getSetterName() + "(" + assoc.getName() + ");\n");
			}
		}

		if (flushAndRefresh) {
			b.append("\nif(flushAndRefresh)\n");
			b.append("{\n");
			b.append("// Call the flush() method in order to force the database insert immediately!\n");
			b.append("em.flush();\n\n");
			b.append("// Get a fully attached version of the entity\n");
			b.append("em.refresh(targetObject);\n");
			b.append("}\n");
		}

		b.append("\nreturn " + TARGET_OBJ_NAME + ";\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getRequiredRepositories()
	 */
	@Override
	public Map<String, Repository> getRequiredRepositories() {
		return requiredRepositories;
	}

}

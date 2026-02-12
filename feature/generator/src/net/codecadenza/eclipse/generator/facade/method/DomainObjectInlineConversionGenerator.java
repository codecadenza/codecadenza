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
package net.codecadenza.eclipse.generator.facade.method;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_GET_REFERENCE;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for converting domain objects directly within a method call
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectInlineConversionGenerator {
	private Set<String> imports;
	private final DTOBean dtoBean;
	private final String targetDomainObject;
	private final String sourceDomainObject;
	private BuildModeEnum mode = BuildModeEnum.NONE;
	private final Project project;

	private enum BuildModeEnum {
		UPDATE, INSERT, NONE
	}

	/**
	 * Constructor
	 * @param methodType
	 * @param dtoBean
	 * @param targetDomainObject
	 * @param sourceDomainObject
	 */
	public DomainObjectInlineConversionGenerator(BoundaryMethodTypeEnumeration methodType, DTOBean dtoBean,
			String targetDomainObject, String sourceDomainObject) {
		this.dtoBean = dtoBean;
		this.targetDomainObject = targetDomainObject;
		this.sourceDomainObject = sourceDomainObject;
		this.imports = new HashSet<>();
		this.project = dtoBean.getNamespace().getProject();

		if (methodType == BoundaryMethodTypeEnumeration.CREATE)
			this.mode = BuildModeEnum.INSERT;

		if (methodType == BoundaryMethodTypeEnumeration.UPDATE)
			this.mode = BuildModeEnum.UPDATE;
	}

	/**
	 * @return all necessary imports for the domain object conversion
	 */
	public Set<String> getImports() {
		imports = new HashSet<>();

		// Just call the method to fill the import map!
		createConversion();

		return imports;
	}

	/**
	 * @param attr
	 * @param targetDomainObject
	 * @param sourceDomainObject
	 * @param getter
	 * @param setter
	 * @return the generated content
	 */
	private String addTypeSpecificSetter(DTOBeanAttribute attr, String targetDomainObject, String sourceDomainObject, String getter,
			String setter) {
		final var b = new StringBuilder();
		final DomainAttribute domainAttribute = attr.getDomainAttribute();
		final String domainObjectClassName = domainAttribute.getDomainObject().getName();
		final boolean optional = domainAttribute.getMinFieldLength().isEmpty();

		// We have to take care for a different handling of attributes that represent a document reference
		if (domainAttribute.getTag() == AttributeTagEnumeration.DOCUMENT_REF) {
			imports.add("import java.io.*;");
			imports.add("import net.codecadenza.runtime.file.*;");

			b.append("\ntry\n");
			b.append("{\n");
			b.append("// Get file we have uploaded earlier\n");
			b.append("final String path = " + sourceDomainObject + "." + getter + ";\n\n");

			if (optional) {
				b.append("if(path != null && !path.isEmpty())\n");
				b.append("{\n");
			}

			b.append("final var temporaryFile = new File(path);\n");
			b.append("final String repositoryPath = FileUtil.getUniqueFileName(");
			b.append(domainObjectClassName + ".class.getSimpleName());\n\n");
			b.append("FileUtil.copyFile(temporaryFile, new File(repositoryPath));\n");
			b.append(targetDomainObject + "." + setter + "(repositoryPath);\n");

			// We must not delete the original file if the application doesn't run in a managed environment!
			if (!project.isJavaSEApplication()) {
				b.append("\n// Delete temporary file after data has been saved in respective field!\n");
				b.append("java.nio.file.Files.delete(temporaryFile.toPath());\n");
			}

			if (optional)
				b.append("}\n");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");
			b.append("throw new FileOperationException(e);\n");
			b.append("}\n\n");
		}
		else if (domainAttribute.convertAttribute()) {
			final String converter = domainAttribute.getConverterExpression();

			b.append(targetDomainObject + "." + setter + "(" + sourceDomainObject + "." + getter);
			b.append(" != null ? " + sourceDomainObject + "." + getter + converter + " : null);\n");
		}
		else if (mode == BuildModeEnum.UPDATE && !project.isJavaSEApplication())
			b.append(targetDomainObject + "." + setter + "(" + sourceDomainObject + "." + getter + ");\n");

		return b.toString();
	}

	/**
	 * @param attr
	 * @return the generated content
	 */
	private String convertAttribute(DTOBeanAttribute attr) {
		final var b = new StringBuilder();
		final DomainAttribute domainAttribute = attr.getDomainAttribute();
		final AbstractDomainAssociation assoc = attr.getAssociation();

		// We do not have to set fields that are maintained by the JPA provider!
		if (mode == BuildModeEnum.UPDATE && domainAttribute.isPk() && assoc == null)
			return b.toString();

		if (domainAttribute.isSetDateOnPersist() || domainAttribute.isSetDateOnUpdate())
			return b.toString();

		// In case of Hibernate the version must be checked manually!
		if (project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE) {
			if (domainAttribute.isTrackVersion())
				return b.toString();
		}
		else if (mode == BuildModeEnum.INSERT && domainAttribute.isTrackVersion()) {
			// If we use EclipseLink we just have to set the version attribute!
			return b.toString();
		}

		// Fields that are not supposed to be updated can be skipped!
		if ((mode == BuildModeEnum.UPDATE && !domainAttribute.isUpdatable())
				|| (mode == BuildModeEnum.INSERT && !domainAttribute.isInsertable()))
			return b.toString();

		// Check if the primary key attribute is filled automatically by the JPA provider
		if (mode == BuildModeEnum.INSERT && domainAttribute.isPk() && assoc == null
				&& domainAttribute.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
			return b.toString();

		if (assoc == null) {
			final String setter = domainAttribute.getSetterName();
			final String getter = domainAttribute.getGetterName();

			b.append(addTypeSpecificSetter(attr, targetDomainObject, sourceDomainObject, getter, setter));
		}
		else if (assoc instanceof OneToOneAssociation) {
			final var setter = assoc.getGetterName() + "." + domainAttribute.getSetterName();
			final var getter = assoc.getGetterName() + "." + domainAttribute.getGetterName();

			b.append(addTypeSpecificSetter(attr, targetDomainObject, sourceDomainObject, getter, setter));
		}

		return b.toString();
	}

	/**
	 * @param attr
	 * @return the generated content
	 */
	private String convertAssociation(DTOBeanAttribute attr) {
		final var b = new StringBuilder();
		final AbstractDomainAssociation assoc = attr.getAssociation();
		final DTOBean dto = attr.getReferencedDTOBean();
		final DTOBeanAttribute pkAttr = dto.getPKAttribute();

		if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
			imports.add("import " + assoc.getTarget().getNamespace().toString() + ".*;");
			imports.add("import java.util.*;");

			final var listName = assoc.getName() + "List";

			b.append("\n");
			b.append("final var " + listName + " = new " + JavaTypeModifierEnumeration.ARRAY_LIST.toString());
			b.append("<" + assoc.getTarget().getName() + ">();\n\n");
			b.append("for(final " + assoc.getTarget().getName() + " a : " + sourceDomainObject + "." + assoc.getGetterName() + ")\n");
			b.append(listName + ".add(" + REPO_METHOD_NAME_GET_REFERENCE);
			b.append("(" + assoc.getTarget().getName() + ".class, a." + pkAttr.getGetterName() + "));\n\n");
			b.append(targetDomainObject + "." + assoc.getSetterName() + "(" + listName + ");\n");
		}
		else if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
			final String setter = attr.getAssociationListGetter() + attr.getAssociation().getSetterName();
			final String getter = attr.getAssociationListGetter() + attr.getAssociation().getGetterName();
			final String nullCheck = attr.getAssociationListNullCheck(targetDomainObject, true);
			final DomainObject target;
			boolean isUpdatable = true;
			boolean isInsertable = true;
			boolean isOptional = false;

			if (assoc instanceof final ManyToOneAssociation mto) {
				isUpdatable = mto.isUpdatable();
				isInsertable = mto.isInsertable();
				target = mto.getTarget();
				isOptional = mto.isOptional();
			}
			else {
				final var oto = (OneToOneAssociation) assoc;
				target = oto.getTarget();
				isOptional = oto.isOptional();
			}

			// Associations that are not supposed to be updated or inserted can be skipped!
			if ((mode == BuildModeEnum.UPDATE && !isUpdatable) || (mode == BuildModeEnum.INSERT && !isInsertable))
				return b.toString();

			imports.add("import " + target.getNamespace().toString() + ".*;");

			if (!nullCheck.isEmpty()) {
				b.append("\n");
				b.append(nullCheck);
				b.append("{\n");
			}

			if (isOptional) {
				if (nullCheck.isEmpty())
					b.append("\n");

				b.append("if(" + sourceDomainObject + "." + getter + " != null)\n");
			}

			b.append(targetDomainObject + "." + setter + "(");
			b.append(REPO_METHOD_NAME_GET_REFERENCE + "(" + assoc.getTarget().getName() + ".class, ");
			b.append(sourceDomainObject + "." + getter + "." + target.getPKAttribute().getGetterName() + "));\n");

			if (isOptional) {
				b.append("else\n");
				b.append(targetDomainObject + "." + setter + "(null);\n");
			}

			if (!nullCheck.isEmpty())
				b.append("}\n\n");
			else if (isOptional)
				b.append("\n");
		}

		return b.toString();
	}

	/**
	 * Create the attribute conversion
	 * @return the generated content
	 */
	public String createConversion() {
		final var b = new StringBuilder();

		dtoBean.getAttributes().forEach(attr -> {
			final DomainAttribute domainAttribute = attr.getDomainAttribute();

			if (domainAttribute != null)
				b.append(convertAttribute(attr));
			else if (!project.isJavaSEApplication())
				b.append(convertAssociation(attr));
		});

		return b.toString();
	}

}

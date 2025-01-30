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
package net.codecadenza.eclipse.generator.dto;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_GET_REFERENCE;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for converting DTOs directly within a method call
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DTOInlineConversionGenerator {
	private HashSet<String> imports;
	private final BoundaryMethodTypeEnumeration methodType;
	private final DTOBean dtoBean;
	private final String domainObjName;
	private final String dtoName;

	private enum BuildModeEnum {
		INSERT, UPDATE, SAVE, NONE
	}

	/**
	 * @param methodType
	 * @param dtoBean
	 * @param domainObjName
	 * @param dtoName
	 */
	public DTOInlineConversionGenerator(BoundaryMethodTypeEnumeration methodType, DTOBean dtoBean, String domainObjName,
			String dtoName) {
		this.methodType = methodType;
		this.dtoBean = dtoBean;
		this.domainObjName = domainObjName;
		this.dtoName = dtoName;
		this.imports = new HashSet<>();
	}

	/**
	 * @return all necessary imports for the DTO to domain object conversion
	 */
	public Set<String> getImports() {
		imports = new HashSet<>();

		// Just call the method to fill the import map!
		addAttributeSetters();

		return imports;
	}

	/**
	 * @return all necessary imports for the domain object to DTO conversion
	 */
	public Set<String> getReverseImports() {
		imports = new HashSet<>();

		// Just call the method to fill the import map!
		addReverseAttributeSetters();

		return imports;
	}

	/**
	 * Add attribute setters
	 * @return the generated content
	 */
	public String addAttributeSetters() {
		BuildModeEnum mode = BuildModeEnum.NONE;

		if (methodType == BoundaryMethodTypeEnumeration.CREATE)
			mode = BuildModeEnum.INSERT;
		else if (methodType == BoundaryMethodTypeEnumeration.UPDATE)
			mode = BuildModeEnum.UPDATE;
		else if (methodType == BoundaryMethodTypeEnumeration.SAVE)
			mode = BuildModeEnum.SAVE;

		return addAttributeSetters(mode, dtoBean, domainObjName, dtoName);
	}

	/**
	 * @param attr
	 * @param domainObjectName
	 * @param dtoName
	 * @param getter
	 * @param setter
	 * @return the generated content
	 */
	private String addTypeSpecificSetter(DTOBeanAttribute attr, String domainObjectName, String dtoName, String getter,
			String setter) {
		final var b = new StringBuilder();
		final DomainAttribute domainAttribute = attr.getDomainAttribute();
		final JavaType type = domainAttribute.getJavaType();
		final String domainObjectClassName = domainAttribute.getDomainObject().getName();
		final Project project = attr.getDTOBean().getNamespace().getProject();
		boolean isMandatory = true;

		// We have to take care regarding the different handling of byte[] and Byte[] attributes!
		if (type.isByteArray() || domainAttribute.getTag() == AttributeTagEnumeration.DOCUMENT_REF) {
			if (domainAttribute.isLob() && domainAttribute.getDomainAttributeValidator().isNullable())
				isMandatory = false;

			if (!domainAttribute.isLob() && domainAttribute.getMinFieldLength().isEmpty())
				isMandatory = false;

			b.append("\ntry\n");
			b.append("{\n");
			b.append("// Get file we have uploaded earlier\n");
			b.append("final String path = " + dtoName + "." + getter + ";\n\n");

			if (!isMandatory) {
				b.append("if(path != null && !path.isEmpty())\n");
				b.append("{\n");
			}

			imports.add("import java.io.*;");
			imports.add("import net.codecadenza.runtime.file.*;");

			b.append("final var temporaryFile = new File(path);\n");

			if (type.isType(JavaType.BYTE_ARRAY))
				b.append(domainObjectName + "." + setter + "(FileUtil.getBytesFromFile(temporaryFile));\n");
			else if (type.isType(JavaType.BYTE_OBJ_ARRAY))
				b.append(domainObjectName + "." + setter + "(FileUtil.convertToByteArray(FileUtil.getBytesFromFile(temporaryFile)));\n");
			else if (type.isString()) {
				// Because of the given type it should be a document reference!
				b.append("final String repositoryPath = FileUtil.getUniqueFileName(");
				b.append(domainObjectClassName + ".class.getSimpleName());\n\n");
				b.append("FileUtil.copyFile(temporaryFile, new File(repositoryPath));\n");
				b.append(domainObjectName + "." + setter + "(repositoryPath);\n");
			}

			// We must not delete the original file if the application doesn't run in a managed environment!
			if (!project.isJavaSEApplication()) {
				b.append("\n// Delete temporary file after data has been saved in respective field!\n");
				b.append("java.nio.file.Files.delete(temporaryFile.toPath());\n");
			}

			if (!isMandatory)
				b.append("}\n");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");
			b.append("throw new FileOperationException(e);\n");
			b.append("}\n\n");
		}
		else {
			b.append(domainObjectName + "." + setter + "(" + dtoName + "." + getter);

			if (domainAttribute.convertAttribute())
				b.append(" != null ? " + dtoName + "." + getter + domainAttribute.getConverterExpression() + " : null");

			b.append(");\n");
		}

		return b.toString();
	}

	/**
	 * Create the attribute setter block
	 * @param mode
	 * @param dtoBean
	 * @param domainObjName
	 * @param dtoName
	 * @return the generated content
	 */
	private String addAttributeSetters(BuildModeEnum mode, DTOBean dtoBean, String domainObjName, String dtoName) {
		final var b = new StringBuilder();
		final var initializedAssocMap = new HashSet<Integer>();
		final Project project = dtoBean.getNamespace().getProject();

		for (final DTOBeanAttribute attr : dtoBean.getAttributes()) {
			final DomainAttribute domainAttribute = attr.getDomainAttribute();
			final AbstractDomainAssociation domainAssoc = attr.getAssociation();

			if (domainAttribute != null) {
				// We do not have to set fields that are maintained by the JPA provider!
				if (mode == BuildModeEnum.UPDATE && domainAttribute.isPk() && attr.getAssociation() == null)
					continue;

				if (domainAttribute.isSetDateOnPersist() || domainAttribute.isSetDateOnUpdate())
					continue;

				// In case of JBoss 4.2.3 with Hibernate 3.2.4 we must check the version manually!
				if (project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE) {
					if (domainAttribute.isTrackVersion())
						continue;
				}
				else if (mode == BuildModeEnum.INSERT && domainAttribute.isTrackVersion()) {
					// If we use EclipseLink we just have to set the version attribute!
					continue;
				}

				// Fields that are not supposed to be updated can be skipped!
				if ((mode == BuildModeEnum.UPDATE && !domainAttribute.isUpdatable())
						|| (mode == BuildModeEnum.INSERT && !domainAttribute.isInsertable()))
					continue;

				// Check if the primary key attribute is filled automatically by the JPA provider
				if ((mode == BuildModeEnum.INSERT || mode == BuildModeEnum.SAVE) && domainAttribute.isPk()
						&& attr.getAssociation() == null
						&& attr.getDomainAttribute().getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
					continue;

				if (domainAssoc == null) {
					final String setter = domainAttribute.getSetterName();
					final String getter = attr.getGetterName();

					b.append(addTypeSpecificSetter(attr, domainObjName, dtoName, getter, setter));
				}
				else if (domainAssoc instanceof OneToOneAssociation) {
					// We assume that this association is not optional in this case!
					if (mode == BuildModeEnum.INSERT && !initializedAssocMap.contains(domainAssoc.hashCode())) {
						b.append(domainObjName + "." + domainAssoc.getSetterName());
						b.append("(new " + domainAssoc.getTarget().getName() + "());\n");

						imports.add("import " + domainAssoc.getTarget().getNamespace().toString() + ".*;");
						initializedAssocMap.add(domainAssoc.hashCode());
					}

					final String setter = domainAssoc.getGetterName() + "." + domainAttribute.getSetterName();
					final String getter = attr.getGetterName();

					b.append(addTypeSpecificSetter(attr, domainObjName, dtoName, getter, setter));
				}
				else if (domainAssoc instanceof final ManyToOneAssociation mto && attr.getDomainAttribute().isPk()) {
					// Associations that are not supposed to be updated or inserted can be skipped!
					if ((mode == BuildModeEnum.UPDATE && !mto.isUpdatable()) || (mode == BuildModeEnum.INSERT && !mto.isInsertable()))
						continue;

					final String assocListNullCheck = attr.getAssociationListNullCheck(domainObjName, true);
					final String domainSetter = attr.getAssociationListGetter() + domainAssoc.getSetterName();
					final String dtoGetter = attr.getGetterName();

					if (!assocListNullCheck.isEmpty()) {
						b.append("\n");
						b.append(assocListNullCheck);
						b.append("{\n");
					}

					if (mto.isOptional()) {
						if (assocListNullCheck.isEmpty())
							b.append("\n");

						b.append("if(");

						if (!attr.getDomainAttribute().getJavaType().isPrimitive())
							b.append(dtoName + "." + dtoGetter + " == null || ");

						b.append(dtoName + "." + dtoGetter);

						if (attr.getDomainAttribute().getJavaType().isString())
							b.append(".isEmpty()");
						else
							b.append(" == " + attr.getDomainAttribute().getEmptyItemDefaultValue());

						b.append(")\n" + domainObjName + "." + domainSetter + "(null);\n");
						b.append("else\n");
					}

					imports.add("import " + domainAssoc.getTarget().getNamespace().toString() + ".*;");

					b.append(domainObjName + "." + domainSetter + "(");
					b.append(DEFAULT_REPOSITORY + "." + REPO_METHOD_NAME_GET_REFERENCE);
					b.append("(" + domainAssoc.getTarget().getName() + ".class, " + dtoName + "." + dtoGetter + ")");
					b.append(");\n");

					if (!assocListNullCheck.isEmpty())
						b.append("}\n\n");
					else if (mto.isOptional())
						b.append("\n");
				}
			}
			else {
				final AbstractDomainAssociation assoc = attr.getAssociation();
				final DTOBean dto = attr.getReferencedDTOBean();
				final DTOBeanAttribute pkAttr = dto.getPKAttribute();

				if (assoc instanceof ManyToManyAssociation || assoc instanceof final OneToManyAssociation oto && !oto.isBidirectional()) {
					final String dtoGetter = attr.getGetterName();

					imports.add("import " + assoc.getTarget().getNamespace().toString() + ".*;");
					imports.add("import java.util.*;");
					imports.add("import " + attr.getReferencedDTOBean().getNamespace().toString() + ".*;");

					if (!initializedAssocMap.contains(assoc.hashCode())) {
						b.append(domainObjName + "." + assoc.getSetterName() + "(new ");
						b.append(JavaTypeModifierEnumeration.ARRAY_LIST.toString() + "<>());\n");

						initializedAssocMap.add(assoc.hashCode());
					}

					b.append("\n");
					b.append("for(final " + attr.getReferencedDTOBean().getName() + " a : " + dtoName + "." + dtoGetter + ")\n");
					b.append("{\n");
					b.append("final " + assoc.getTarget().getName() + " b = ");
					b.append(DEFAULT_REPOSITORY + "." + REPO_METHOD_NAME_GET_REFERENCE);
					b.append("(" + assoc.getTarget().getName() + ".class, a." + pkAttr.getGetterName() + ");\n");
					b.append(domainObjName + "." + assoc.getGetterName() + ".add(b);\n");
					b.append("}\n\n");
				}
				else if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
					boolean isUpdatable = true;
					boolean isInsertable = true;
					boolean isOptional = false;

					if (assoc instanceof final ManyToOneAssociation mto) {
						isUpdatable = mto.isUpdatable();
						isInsertable = mto.isInsertable();
						isOptional = mto.isOptional();
					}
					else {
						final var oto = (OneToOneAssociation) assoc;
						isOptional = oto.isOptional();
					}

					// Associations that are not supposed to be updated or inserted can be skipped!
					if ((mode == BuildModeEnum.UPDATE && !isUpdatable) || (mode == BuildModeEnum.INSERT && !isInsertable))
						continue;

					final String domainSetter = attr.getAssociationListGetter() + attr.getAssociation().getSetterName();
					final String dtoGetter = attr.getGetterName() + "." + pkAttr.getGetterName();
					final String assocListNullCheck = attr.getAssociationListNullCheck(domainObjName, true);

					if (!assocListNullCheck.isEmpty()) {
						b.append("\n");
						b.append(assocListNullCheck);
						b.append("{\n");
					}

					if (isOptional) {
						final DTOBeanAttribute refDTOPKAttr = attr.getReferencedDTOBean().getPKAttribute();
						final JavaType refPkType = refDTOPKAttr.getDomainAttribute().getJavaType();

						if (assocListNullCheck.isEmpty())
							b.append("\n");

						b.append("if(" + dtoName + "." + attr.getGetterName() + " == null || ");
						b.append(dtoName + "." + attr.getGetterName() + "." + refDTOPKAttr.getGetterName());

						if (refPkType.isString())
							b.append(".isEmpty()");
						else
							b.append(" == " + refDTOPKAttr.getDomainAttribute().getEmptyItemDefaultValue());

						b.append(")\n" + domainObjName + "." + domainSetter + "(null);\n");
						b.append("else\n");
					}

					b.append(domainObjName + "." + domainSetter + "(");

					imports.add("import " + assoc.getTarget().getNamespace().toString() + ".*;");

					b.append(DEFAULT_REPOSITORY + "." + REPO_METHOD_NAME_GET_REFERENCE);
					b.append("(" + assoc.getTarget().getName() + ".class, " + dtoName + "." + dtoGetter + ")");
					b.append(");\n");

					if (!assocListNullCheck.isEmpty())
						b.append("}\n\n");
					else if (isOptional)
						b.append("\n");
				}
			}
		}

		return b.toString();
	}

	/**
	 * Create the attribute setter block
	 * @return the generated content
	 */
	public String addReverseAttributeSetters() {
		final var b = new StringBuilder();

		for (final DTOBeanAttribute attr : dtoBean.getAttributes()) {
			final DomainAttribute domainAttribute = attr.getDomainAttribute();
			final String dtoSetter = attr.getSetterName();

			if (domainAttribute != null) {
				final AbstractDomainAssociation domainAssoc = attr.getAssociation();

				if (domainAssoc == null) {
					final String domainGetter = domainAttribute.getGetterName();

					b.append(dtoName + "." + dtoSetter + "(" + domainObjName + "." + domainGetter + ");\n");
				}
				else {
					// In this mode we have to take care of deeply cascaded association lists in order to prevent NullPointerExceptions
					// when accessing optional associations!
					final String nullCheck = attr.getNullCheck(domainObjName, null);
					final String getter = attr.getAssociationListGetter() + domainAssoc.getGetterName() + "."
							+ domainAttribute.getGetterName();

					if (!nullCheck.isEmpty()) {
						b.append("\n");
						b.append(nullCheck);
					}

					b.append(dtoName + "." + dtoSetter + "(" + domainObjName + "." + getter + ");\n");

					if (!nullCheck.isEmpty()) {
						if (domainAssoc instanceof final ManyToOneAssociation mto && mto.isOptional() && domainAttribute.isPk()
								&& domainAttribute.getJavaType().isIntegerOrLong() && attr.getSearchType().isPrimitive()) {
							// Set the respective default value for attributes of type int or long indicating that the association is null!
							b.append("else\n");
							b.append(dtoName + "." + dtoSetter + "(" + domainAttribute.getEmptyItemDefaultValue() + ");\n");
						}

						b.append("\n");
					}
				}
			}
			else {
				final AbstractDomainAssociation assoc = attr.getAssociation();
				final String dtoGetter = attr.getGetterName();
				final String domainGetter = attr.getAssociationListGetter() + assoc.getGetterName();
				final String nullCheck = attr.getNullCheck(domainObjName, null);
				final DTOBean refDTO = attr.getReferencedDTOBean();

				if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
					if (!nullCheck.isEmpty()) {
						b.append("\n");
						b.append(nullCheck);
						b.append("{\n");
					}

					b.append(dtoName + "." + dtoSetter + "(new " + refDTO.getName() + "());\n");

					imports.add("import " + refDTO.getNamespace().toString() + ".*;");

					// We just fill all list DTO attributes that are mapped to domain attributes!
					for (final DTOBeanAttribute refAttr : attr.getReferencedDTOBean().getAttributes()) {
						if (refAttr.getDomainAttribute() == null || refAttr.getAssociation() != null)
							continue;

						final DomainAttribute refDomainAttr = refAttr.getDomainAttribute();
						final String refDTOSetter = refAttr.getSetterName();
						final String refDomainGetter = refDomainAttr.getGetterName();

						b.append(dtoName + "." + dtoGetter + "." + refDTOSetter + "(" + domainObjName + "." + domainGetter);
						b.append("." + refDomainGetter + ");\n");
					}

					if (!nullCheck.isEmpty())
						b.append("}\n\n");
				}
				else if (assoc instanceof ManyToManyAssociation
						|| assoc instanceof final OneToManyAssociation oto && !oto.isBidirectional()) {
					imports.add("import " + assoc.getTarget().getNamespace().toString() + ".*;");
					imports.add("import java.util.*;");
					imports.add("import " + refDTO.getNamespace().toString() + ".*;");

					b.append("\n");
					b.append(dtoName + "." + dtoSetter + "(new ArrayList<>());\n\n");
					b.append("for(final " + assoc.getTarget().getName() + " listElement : ");
					b.append(domainObjName + "." + domainGetter + ")\n");
					b.append("{\n");
					b.append("final var listDTO = new " + refDTO.getName() + "();\n");

					// We just fill all list DTO attributes that are mapped to domain attributes!
					for (final DTOBeanAttribute refAttr : refDTO.getAttributes()) {
						if (refAttr.getDomainAttribute() == null || refAttr.getAssociation() != null)
							continue;

						final DomainAttribute refDomainAttr = refAttr.getDomainAttribute();
						final String refDTOSetter = refAttr.getSetterName();
						final String refDomainGetter = refDomainAttr.getGetterName();

						b.append("listDTO." + refDTOSetter + "(listElement." + refDomainGetter + ");\n");
					}

					b.append("\n");
					b.append(dtoName + "." + dtoGetter + ".add(listDTO);\n");
					b.append("}\n\n");
				}
			}
		}

		return b.toString();
	}

	/**
	 * Fill all fields that are managed by the persistence provider after performing a persist operation
	 * @return the generated content
	 */
	public String setManagedFieldsAfterPersist() {
		final var b = new StringBuilder();

		for (final DTOBeanAttribute attr : dtoBean.getAttributes()) {
			if (attr.getDomainAttribute() == null || attr.getAssociation() != null)
				continue;

			if (attr.getDomainAttribute().isTrackVersion() || attr.getDomainAttribute().isSetDateOnPersist()) {
				final String setter = attr.getSetterName();
				final String getter = attr.getDomainAttribute().getGetterName();

				b.append(dtoName + "." + setter + "(" + domainObjName + "." + getter + ");\n");
			}
		}

		return b.toString();
	}

	/**
	 * Fill all fields that are managed by the persistence provider after performing a merge operation
	 * @return the generated content
	 */
	public String setManagedFieldsAfterMerge() {
		final var b = new StringBuilder();

		for (final DTOBeanAttribute attr : dtoBean.getAttributes()) {
			if (attr.getDomainAttribute() == null || attr.getAssociation() != null)
				continue;

			if (attr.getDomainAttribute().isTrackVersion() || attr.getDomainAttribute().isSetDateOnUpdate()) {
				final String setter = attr.getSetterName();
				final String getter = attr.getDomainAttribute().getGetterName();

				b.append(dtoName + "." + setter + "(" + domainObjName + "." + getter + ");\n");
			}
		}

		return b.toString();
	}

}

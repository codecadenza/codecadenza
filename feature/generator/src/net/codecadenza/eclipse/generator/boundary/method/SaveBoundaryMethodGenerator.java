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

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_MERGE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_PERSIST;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for boundary methods that perform a save operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SaveBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	private final RepositoryMethod repositoryMethod;
	private final Repository repository;
	private final String objectToBeSaved;
	private final DTOInlineConversionGenerator converter;
	private DTOBean dtoBean;
	private String returnObjectName;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public SaveBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof final DTOBean dto) {
				this.dtoBean = dto;
				this.returnObjectName = p.getName();
				break;
			}

		this.repositoryMethod = (RepositoryMethod) method.getServiceMethod();
		this.repository = repositoryMethod.getRepository();
		this.objectToBeSaved = dtoBean.getDomainObject().getLowerCaseName() + "ToSave";
		this.converter = new DTOInlineConversionGenerator(method.getMethodType(), dtoBean, objectToBeSaved, returnObjectName);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment());
		b.append(" * @param " + returnObjectName + " the " + domainObjectLabel + " to be saved\n");

		if (performsUniqueCheckOnPersist() || performsUniqueCheckOnMerge())
			b.append(" * @throws UniqueConstraintViolationException if a unique constraint check has failed\n");

		if (!validationExceptionComment.isEmpty())
			b.append(validationExceptionComment);

		b.append(" * @return the saved " + domainObjectLabel + " object\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final String typeToBeSaved = dtoBean.getDomainObject().getName();
		final DTOBeanAttribute pkDTOAttr = dtoBean.getPKAttribute();
		final IDGeneratorTypeEnumeration idGeneratorType = dtoBean.getDomainObject().getIDGenerator().getGeneratorType();

		if (idGeneratorType == IDGeneratorTypeEnumeration.NONE) {
			// In any case, the primary key attribute must be present!
			b.append("// Find and attach the persistent object\n");
			b.append(typeToBeSaved + " " + objectToBeSaved + " = " + getRepositoryName() + repositoryMethod.getName() + "(");
			b.append(returnObjectName + "." + pkDTOAttr.getGetterName() + ");\n");
			b.append("final boolean createNew = " + objectToBeSaved + " == null;\n\n");
		}
		else {
			// Skip trying to find the object if the primary key attribute has just a default value (e.g. null or 0)!
			b.append("boolean createNew = true;\n");
			b.append(typeToBeSaved + " " + objectToBeSaved + " = null;\n\n");
			b.append("if(" + returnObjectName + "." + pkDTOAttr.getGetterName() + " != ");
			b.append(pkDTOAttr.getDomainAttribute().getJavaType().getLocalVariableDefaultValue() + ")\n");
			b.append("{\n");
			b.append("// Find and attach the persistent object\n");
			b.append(objectToBeSaved + " = " + getRepositoryName() + repositoryMethod.getName() + "(");
			b.append(returnObjectName + "." + pkDTOAttr.getGetterName() + ");\n");
			b.append("createNew = " + objectToBeSaved + " == null;\n");
			b.append("}\n\n");
		}

		b.append("if(createNew)\n");
		b.append(objectToBeSaved + " = new " + typeToBeSaved + "();\n\n");
		b.append(converter.addAttributeSetters());
		b.append("\n");
		b.append("if(createNew)\n");
		b.append("{\n");
		b.append("// Persist a new object\n");
		b.append(addPersistCall());
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("// Merge the existing object\n");
		b.append(addMergeCall());
		b.append("}\n\n");

		if (addTransactionManagement)
			b.append("tr.commit();\n\n");

		b.append("return " + returnObjectName + ";\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.addAll(converter.getImports());

		if (!validationExceptionImport.isEmpty())
			imports.add(validationExceptionImport);

		if (performsUniqueCheckOnPersist() || performsUniqueCheckOnMerge())
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());

		if (!validationExceptionImport.isEmpty())
			imports.add(validationExceptionImport);

		if (performsUniqueCheckOnPersist() || performsUniqueCheckOnMerge())
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

	/**
	 * Create the persist method call
	 * @return the generated content
	 */
	private String addPersistCall() {
		final var b = new StringBuilder();
		final RepositoryMethod persistMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST);
		final String checkParam = performsUniqueCheckOnPersist() ? ", true" : "";
		final DTOBeanAttribute pkDTOAttr = dtoBean.getPKAttribute();
		final String methodName = persistMethod != null ? persistMethod.getName() : REPO_METHOD_NAME_PERSIST;
		final var afterPersist = new StringBuilder();

		afterPersist.append(converter.setManagedFieldsAfterPersist());

		if (dtoBean.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE) {
			// Fill the auto-generated primary key attribute
			final String setter = pkDTOAttr.getSetterName();
			final String getter = pkDTOAttr.getDomainAttribute().getGetterName();

			afterPersist.append(returnObjectName + "." + setter + "(" + objectToBeSaved + "." + getter + ");\n");
		}

		if (!afterPersist.isEmpty())
			b.append(objectToBeSaved + " = ");

		b.append(getRepositoryName() + methodName);
		b.append("(" + objectToBeSaved + checkParam + ", true, true);\n");

		if (!afterPersist.isEmpty()) {
			b.append("\n");
			b.append(afterPersist.toString());
		}

		return b.toString();
	}

	/**
	 * Create the merge method call
	 * @return the generated content
	 */
	private String addMergeCall() {
		final var b = new StringBuilder();
		final RepositoryMethod mergeMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.MERGE);
		final String checkParam = performsUniqueCheckOnMerge() ? ", true" : "";
		final String methodName = mergeMethod != null ? mergeMethod.getName() : REPO_METHOD_NAME_MERGE;
		final String afterMerge = converter.setManagedFieldsAfterMerge();

		if (!afterMerge.isEmpty())
			b.append(objectToBeSaved + " = ");

		b.append(getRepositoryName() + methodName);
		b.append("(" + objectToBeSaved + checkParam + ", true);\n");

		if (!afterMerge.isEmpty()) {
			b.append("\n");
			b.append(afterMerge);
		}

		return b.toString();
	}

	/**
	 * @return true if the persist method performs a unique check
	 */
	private boolean performsUniqueCheckOnPersist() {
		return repository.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST) != null
				&& repository.getMethodByType(RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY) != null;
	}

	/**
	 * @return true if the merge method performs a unique check
	 */
	private boolean performsUniqueCheckOnMerge() {
		return repository.getMethodByType(RepositoryMethodTypeEnumeration.MERGE) != null
				&& repository.getMethodByType(RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID) != null;
	}

}

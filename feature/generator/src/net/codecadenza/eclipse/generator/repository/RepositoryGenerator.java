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
package net.codecadenza.eclipse.generator.repository;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.facade.FacadeGenerator;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.RepositoryMethodGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repositories
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryGenerator extends AbstractJavaSourceGenerator {
	private final Repository repository;
	private final Project project;
	private final Map<String, Repository> additionalRepositories = new HashMap<>();
	private final Map<String, String> queryParameterConstants = new HashMap<>();

	/**
	 * Constructor
	 * @param repository
	 */
	public RepositoryGenerator(Repository repository) {
		super(repository.getSourceFile());

		this.repository = repository;
		this.project = repository.getNamespace().getProject();

		findAdditionalFields();
	}

	/**
	 * Constructor for using the repository generator functionality in the {@link FacadeGenerator}
	 * @param boundaryBean
	 */
	public RepositoryGenerator(BoundaryBean boundaryBean) {
		this.repository = boundaryBean.getRepository();
		this.project = this.repository.getNamespace().getProject();

		findAdditionalFields();
	}

	/**
	 * @return a list with all additional repositories
	 */
	public List<Repository> getAdditionalRepositories() {
		return additionalRepositories.values().stream().toList();
	}

	/**
	 * @return a {@link Map} with all necessary query parameter constants
	 */
	public Map<String, String> getQueryParameterConstants() {
		return queryParameterConstants;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("net.codecadenza.runtime.jpa");
		importPackage(repository.getDomainObject().getNamespace().toString());

		if (project.isJakartaEEApplication())
			importPackage("jakarta.ejb");
		else if (project.isSpringBootApplication())
			importClass("org.springframework.stereotype.Repository");
		else
			importPackage("jakarta.persistence");

		if (!repository.getDomainObject().isAbstract()) {
			if (project.getValidationType() == ValidationTypeEnumeration.INTERNAL)
				importPackage("net.codecadenza.runtime.validation");
			else
				importPackage("jakarta.validation");
		}

		for (final Entry<String, Repository> entrySet : additionalRepositories.entrySet()) {
			if (entrySet.getValue().getNamespace().toString().equals(repository.getNamespace().toString()))
				continue;

			importPackage(entrySet.getValue().getNamespace().toString());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		final JavaType pkType = repository.getDomainObject().getPKAttribute().getJavaType();

		if (project.isJakartaEEApplication())
			b.append("@Stateless\n");
		else if (project.isSpringBootApplication())
			b.append("@Repository\n");

		if (pkType.getNamespace() != null)
			importPackage(pkType.getNamespace().toString());

		b.append("public class " + repository.getName() + " extends AbstractRepository");
		b.append("<" + repository.getDomainObject().getName() + ", ");
		b.append(pkType.getWrapperTypeName());
		b.append(">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		for (final Entry<String, String> entrySet : queryParameterConstants.entrySet())
			addPrivateConstant(JavaType.STRING, entrySet.getKey(), "\"" + entrySet.getValue() + "\"").create();

		for (final Entry<String, Repository> entrySet : additionalRepositories.entrySet()) {
			final Repository repo = entrySet.getValue();

			if (project.isJakartaEEApplication() || project.isSpringBootApplication())
				addPrivateField(repo.getName(), entrySet.getKey()).inject().create();
			else
				addPrivateField(repo.getName(), entrySet.getKey()).create();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		if (project.isJavaSEApplication()) {
			final var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" * @param em\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + repository.getName() + "(EntityManager em)\n");
			b.append("{\n");
			b.append("super(em);\n");

			if (!additionalRepositories.isEmpty())
				b.append("\n");

			for (final Entry<String, Repository> entrySet : additionalRepositories.entrySet())
				b.append("this." + entrySet.getKey() + " = new " + entrySet.getValue().getName() + "(em);\n");

			b.append("}\n\n");

			addConstructor(repository.getName() + "(EntityManager em)", b.toString());
		}
		else
			super.addConstructors();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		repository.getRepositoryMethods().stream().filter(method -> !method.isGenerationOmitted()).forEach(method -> {
			final BasicRepositoryMethodGenerator generator = RepositoryMethodGeneratorFactory.getMethodGenerator(method, this);

			addMethod(generator.getMethodSignature(), generator.createMethod());

			addImports(generator.getImports());
		});
	}

	/**
	 * Find the additional repositories and the required query parameter constants
	 */
	public void findAdditionalFields() {
		repository.getRepositoryMethods().stream().filter(m -> m.getMethodType() == RepositoryMethodTypeEnumeration.COPY)
				.forEach(method -> {
					final BasicRepositoryMethodGenerator methodGenerator = RepositoryMethodGeneratorFactory.getMethodGenerator(method,
							this);
					methodGenerator.createMethod();

					additionalRepositories.putAll(methodGenerator.getRequiredRepositories());
				});

		repository.getRepositoryMethods().stream().forEach(method -> {
			final RepositoryMethodTypeEnumeration methodType = method.getMethodType();

			if (!project.isBoundaryMode()
					&& (method.isGenerationOmitted() || methodType == RepositoryMethodTypeEnumeration.GET_ASSOCIATION))
				return;

			final BasicRepositoryMethodGenerator methodGenerator = RepositoryMethodGeneratorFactory.getMethodGenerator(method, this);
			methodGenerator.createMethod();

			queryParameterConstants.putAll(methodGenerator.getQueryParameterConstants());
		});
	}

}

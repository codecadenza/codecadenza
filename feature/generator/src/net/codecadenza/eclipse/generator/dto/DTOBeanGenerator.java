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

import java.util.ArrayList;
import java.util.Collection;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.XMLMappingType;
import net.codecadenza.eclipse.shared.Constants;

/**
 * <p>
 * Generator for data transfer objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DTOBeanGenerator extends AbstractJavaSourceGenerator {
	private static final String COLLECTION = JavaTypeModifierEnumeration.COLLECTION.toString();

	private final DTOBean dto;
	private final Project project;

	/**
	 * Constructor
	 * @param dto
	 */
	public DTOBeanGenerator(DTOBean dto) {
		super(dto.getSourceFile());

		this.dto = dto;
		this.project = dto.getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importClass("java.io.Serializable");

		dto.getAttributes().forEach(attr -> {
			if (attr.getDomainAttribute() != null) {
				if (attr.getDomainAttribute().getJavaType().getNamespace() != null
						&& !Constants.PACK_JAVA_LANG.equals(attr.getDomainAttribute().getJavaType().getNamespace().toString())) {
					// Add imports for enumerations and date values
					importPackage(attr.getDomainAttribute().getJavaType().getNamespace().toString());
				}
			}
			else if (!attr.getDTOBean().getNamespace().equals(attr.getReferencedDTOBean().getNamespace())) {
				// Add imports for referenced DTOs
				importPackage(attr.getReferencedDTOBean().getNamespace().toString());
			}

			if (attr.getAssociation() != null) {
				// Add java.util if lists are necessary
				final AbstractDomainAssociation assoc = attr.getAssociation();

				if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
					importPackage(Constants.PACK_JAVA_UTIL);
			}
		});

		if (dto.addJAXBAnnotations())
			importPackage("jakarta.xml.bind.annotation");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (dto.addJAXBAnnotations()) {
			b.append("@XmlRootElement");

			if (project.getXmlNamespacePrefix() != null && !project.getXmlNamespacePrefix().isEmpty())
				b.append("(namespace=\"" + project.getXmlNamespace() + "\")");

			b.append("\n");
			b.append("@XmlAccessorType(XmlAccessType.FIELD)\n");
		}

		b.append("public class " + dto.getName() + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		// We don't handle cases where upper-case attribute names are not unique!
		dto.getAttributes().forEach(
				attr -> addPublicConstant(JavaType.STRING, attr.getAttributeNameConstantName(), "\"" + attr.getName() + "\"").create());

		dto.getAttributes().stream().filter(attr -> attr.getSelectToken() != null && !attr.getSelectToken().isEmpty()).forEach(
				attr -> addPublicConstant(JavaType.STRING, attr.getSelectTokenConstantName(), "\"" + attr.getSelectToken() + "\"")
						.create());

		dto.getAttributes().forEach(attr -> {
			final JavaFieldGenerator fieldGenerator;

			if (attr.getDomainAttribute() != null) {
				final JavaType type = attr.getSearchType();

				if (type.isByteArray())
					fieldGenerator = addPrivateField(JavaType.STRING, attr.getName());
				else
					fieldGenerator = addPrivateField(type.getName(), attr.getName());

				if (dto.addJAXBAnnotations() && project.getDefaultXMLMappingType() == XMLMappingType.ATTRIBUTE) {
					fieldGenerator.withAnnotations("@XmlAttribute\n");

					if (type.isLocalDate() || type.isLocalDateTime()) {
						importPackage("jakarta.xml.bind.annotation.adapters");
						importPackage("net.codecadenza.runtime.jaxb");

						if (type.isLocalDateTime())
							fieldGenerator
									.withAnnotations("@XmlJavaTypeAdapter(type = LocalDateTime.class, value = LocalDateTimeAdapter.class)\n");
						else
							fieldGenerator.withAnnotations("@XmlJavaTypeAdapter(type = LocalDate.class, value = LocalDateAdapter.class)\n");
					}
				}
			}
			else {
				final AbstractDomainAssociation assoc = attr.getAssociation();

				if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
					final String refDTOName = attr.getReferencedDTOBean().getName();

					fieldGenerator = addPrivateField(COLLECTION + "<" + refDTOName + ">", attr.getName());
					fieldGenerator.withDefaultValue("new " + JavaTypeModifierEnumeration.ARRAY_LIST.toString() + "<>()");
				}
				else
					fieldGenerator = addPrivateField(attr.getReferencedDTOBean().getName(), attr.getName());

				if (dto.addJAXBAnnotations())
					fieldGenerator.withAnnotations("@XmlElement\n");
			}

			fieldGenerator.create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		// Add the default constructor
		var attrList = new ArrayList<DTOBeanAttribute>();

		addConstructor(attrList, "Default constructor");

		// Add a constructor for the primary key attribute
		attrList = new ArrayList<>();
		attrList.add(dto.getPKAttribute());

		addConstructor(attrList, "Constructor with ID attribute");

		// Add a constructor for the display attribute
		if (dto.getDisplayAttribute() != null) {
			attrList = new ArrayList<>();

			for (final DTOBeanAttribute attr : dto.getAttributes()) {
				if (attr.equals(dto.getPKAttribute()))
					attrList.add(attr);

				if (attr.equals(dto.getDisplayAttribute()))
					attrList.add(attr);
			}

			addConstructor(attrList, "Constructor with ID and display attribute");
		}

		// Add a constructor for all simple attributes. Those attributes that reference other DTOs will be skipped!
		attrList = new ArrayList<>();
		boolean addConstructor = false;

		// Test if the constructor should be added at all as we don't want to have duplicates!
		for (final DTOBeanAttribute attr : dto.getAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			attrList.add(attr);

			if (dto.getDisplayAttribute() == null) {
				if (!attr.equals(dto.getPKAttribute()))
					addConstructor = true;
			}
			else if (!attr.equals(dto.getPKAttribute()) && !attr.equals(dto.getDisplayAttribute()))
				addConstructor = true;
		}

		if (addConstructor)
			addConstructor(attrList, "Constructor using fields");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		StringBuilder b;

		// Add all getters and setters
		for (final DTOBeanAttribute attr : dto.getAttributes()) {
			if (attr.getDomainAttribute() != null) {
				final JavaType type = attr.getSearchType();

				// Add the getter
				if (type.isByteArray()) {
					b = new StringBuilder();
					b.append(createComment(attr, true));
					b.append(getAnnotationForGeneratedElement());
					b.append("public " + JavaType.STRING + " " + attr.getGetterName() + "\n{\nreturn this." + attr.getName() + ";\n}\n\n");

					addMethod(JavaType.STRING + " " + attr.getGetterName(), b.toString());
				}
				else {
					b = new StringBuilder();
					b.append(createComment(attr, true));
					b.append(getAnnotationForGeneratedElement());
					b.append("public " + type.getName() + " " + attr.getGetterName());
					b.append("\n{\nreturn this." + attr.getName() + ";\n}\n\n");

					addMethod(attr.getDomainAttribute().getJavaType().getName() + " " + attr.getGetterName(), b.toString());
				}

				// Add the setter
				if (type.isByteArray()) {
					b = new StringBuilder();
					b.append(createComment(attr, false));
					b.append(getAnnotationForGeneratedElement());
					b.append("public void " + attr.getSetterName() + "(" + JavaType.STRING + " " + attr.getName());
					b.append(")\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

					addMethod("void " + attr.getSetterName() + "(" + JavaType.STRING + " " + attr.getName() + ")", b.toString());
				}
				else {
					b = new StringBuilder();
					b.append(createComment(attr, false));
					b.append(getAnnotationForGeneratedElement());
					b.append("public void " + attr.getSetterName() + "(");
					b.append(type.getName() + " " + attr.getName());
					b.append(")\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

					addMethod("void " + attr.getSetterName() + "(" + attr.getDomainAttribute().getJavaType().getName() + " "
							+ attr.getName() + ")", b.toString());
				}
			}
			else {
				final AbstractDomainAssociation assoc = attr.getAssociation();

				if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
					final String refDTOName = attr.getReferencedDTOBean().getName();

					b = new StringBuilder();
					b.append(createComment(attr, true));
					b.append(getAnnotationForGeneratedElement());
					b.append("public " + COLLECTION + "<" + refDTOName + "> " + attr.getGetterName());
					b.append("\n{\nreturn this." + attr.getName() + ";\n}\n\n");

					addMethod(COLLECTION + "<" + refDTOName + "> " + attr.getGetterName(), b.toString());

					b = new StringBuilder();
					b.append(createComment(attr, false));
					b.append(getAnnotationForGeneratedElement());
					b.append("public void " + attr.getSetterName() + "(" + COLLECTION + "<" + refDTOName + "> ");
					b.append(attr.getName() + ")\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

					addMethod("void " + attr.getSetterName() + "(" + COLLECTION + "<" + refDTOName + "> " + attr.getName() + ")",
							b.toString());
				}
				else {
					b = new StringBuilder();
					b.append(createComment(attr, true));
					b.append(getAnnotationForGeneratedElement());
					b.append("public " + attr.getReferencedDTOBean().getName() + " " + attr.getGetterName());
					b.append("\n{\nreturn this." + attr.getName() + ";\n}\n\n");

					addMethod(attr.getReferencedDTOBean().getName() + " " + attr.getGetterName(), b.toString());

					b = new StringBuilder();
					b.append(createComment(attr, false));
					b.append(getAnnotationForGeneratedElement());
					b.append("public void " + attr.getSetterName() + "(" + attr.getReferencedDTOBean().getName());
					b.append(" " + attr.getName() + ")\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

					addMethod("void " + attr.getSetterName() + "(" + attr.getReferencedDTOBean().getName() + " " + attr.getName() + ")",
							b.toString());
				}
			}
		}

		final DTOBeanAttribute pkDTOAttr = dto.getPKAttribute();

		if (pkDTOAttr == null)
			throw new IllegalStateException("The primary key attribute of the DTO '" + dto.getName() + "' could not be found!");

		final JavaType pkType = pkDTOAttr.getDomainAttribute().getJavaType();
		final String methodName = pkDTOAttr.getGetterName();
		final DTOBeanAttribute displayAttr = dto.getDisplayAttribute();
		boolean addDisplayAttr = false;
		b = new StringBuilder();

		// In case of JSF clients we also have to use the display attribute for equality check. If we don't, corresponding converters
		// (jakarta.faces.convert.Converter) will fail as a converted object cannot be found in the corresponding source list! We
		// don't want to add this feature for other technologies (e.g. Swing or Eclipse RCP) as it is likely to cause serious side
		// effects.
		if (project.hasJSFClient() && displayAttr != null)
			addDisplayAttr = true;

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.lang.Object#equals(java.lang.Object)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public boolean equals(Object obj)\n{\n");
		b.append("if(this == obj)\nreturn true;\n\n");
		b.append("if(obj == null)\nreturn false;\n\n");
		b.append("if(getClass() != obj.getClass())\nreturn false;\n\n");
		b.append("final var dto = (" + dto.getName() + ") obj;\n\n");

		if (!pkType.isPrimitive()) {
			b.append("if(this." + pkDTOAttr.getName() + " == null)\n");
			b.append("{\n");
			b.append("if(dto." + methodName + " != null)\n");
			b.append("return false;\n");
			b.append("}\n");
			b.append("else if(!this." + pkDTOAttr.getName() + ".equals(dto." + methodName + ")");

			if (addDisplayAttr)
				b.append(" && !this." + displayAttr.getName() + ".equals(dto." + displayAttr.getGetterName() + ")");

			b.append(")\n");
			b.append("return false;\n\n");
			b.append("return true;\n");
		}
		else {
			b.append("return this." + pkDTOAttr.getName() + " == dto." + methodName);

			if (addDisplayAttr)
				b.append(" && this." + displayAttr.getName() + ".equals(dto." + displayAttr.getGetterName() + ")");

			b.append(";\n");
		}

		b.append("}\n\n");

		addMethod("boolean equals(Object obj)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.lang.Object#hashCode()\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public int hashCode()\n{\n");

		if (pkDTOAttr.getDomainAttribute().getJavaType().isPrimitive()) {
			if (pkType.isType(JavaType.INT))
				b.append("return " + pkDTOAttr.getName() + ";\n");
			else
				b.append("return (int) (" + pkDTOAttr.getName() + " ^ (" + pkDTOAttr.getName() + " >>> 32));\n");
		}
		else
			b.append("return (" + pkDTOAttr.getName() + " == null) ? 0 : " + pkDTOAttr.getName() + ".hashCode();\n");

		b.append("}\n\n");

		addMethod("int hashCode()", b.toString());
	}

	/**
	 * Add all constructors
	 * @param attributes
	 * @param comment
	 */
	private void addConstructor(Collection<DTOBeanAttribute> attributes, String comment) {
		final var b = new StringBuilder();
		final var allParameters = new StringBuilder();
		final var allAttributes = new StringBuilder();
		final var allCommentParameters = new StringBuilder();
		boolean firstParam = true;

		for (final DTOBeanAttribute attr : attributes) {
			if (attr.getDomainAttribute() == null)
				continue;

			final JavaType type = attr.getSearchType();

			if (firstParam)
				firstParam = false;
			else
				allParameters.append(", ");

			// Attributes of type byte[] or Byte[] must be handled in a special way!
			if (type.isByteArray())
				allParameters.append(JavaType.STRING + " " + attr.getName());
			else
				allParameters.append(type.getName() + " " + attr.getName());

			allAttributes.append("this." + attr.getName() + " = " + attr.getName() + ";\n");
			allCommentParameters.append(" * @param " + attr.getName() + "\n");
		}

		b.append("/**\n");
		b.append(" * " + comment + "\n");
		b.append(allCommentParameters + " */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + dto.getName() + "(" + allParameters + ")\n");
		b.append("{\n");
		b.append(allAttributes);
		b.append("}\n\n");

		addConstructor(dto.getName() + "(" + allParameters + ")", b.toString());
	}

	/**
	 * @param attr
	 * @param getter
	 * @return the generated content
	 */
	private String createComment(DTOBeanAttribute attr, boolean getter) {
		final var comment = new StringBuilder("/**\n");
		comment.append(getter ? " * @return " : " * @param " + attr.getName() + " ");

		if (attr.getDomainAttribute() != null) {
			final JavaType type = attr.getDomainAttribute().getJavaType();

			if (type.isBoolean()) {
				if (attr.getAssociation() == null) {
					if (getter)
						comment.append("true if the " + attr.getDomainAttribute().getLabel() + " flag is set");
					else
						comment.append("the value of the " + attr.getDomainAttribute().getLabel() + " flag to set");
				}
				else if (getter) {
					comment.append("true if the " + attr.getDomainAttribute().getLabel() + " flag of the ");
					comment.append(attr.getAssociation().getTarget().getLabel() + " is set");
				}
				else
					comment.append("the value of the " + attr.getDomainAttribute().getLabel() + " flag to set");
			}
			else {
				if (attr.getAssociation() == null) {
					comment.append("the " + attr.getDomainAttribute().getLabel());
				}
				else {
					comment.append("the " + attr.getDomainAttribute().getLabel() + " of the ");
					comment.append(attr.getAssociation().getTarget().getLabel());
				}

				if (!getter)
					comment.append(" to set");
			}
		}
		else {
			final AbstractDomainAssociation assoc = attr.getAssociation();

			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
				if (getter)
					comment.append("a collection of " + assoc.getTarget().getLabelPlural());
				else
					comment.append("the " + assoc.getTarget().getLabelPlural() + " to set");
			}
			else {
				comment.append("the " + assoc.getTarget().getLabel());

				if (!getter)
					comment.append(" to set");
			}
		}

		comment.append("\n */\n");

		return comment.toString();
	}

}

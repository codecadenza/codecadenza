import { TreeNode } from 'primeng/api';

/**
 * This class represents a specialization of the TreeNode interface and simplifies the
 * intitialization of the tree navigator component.
 */
export class TreeNavigatorNode implements TreeNode<string> {
  static readonly FORM_GROUP_TYPE = 'formgroup';
  static readonly FORM_TYPE = 'form';

  label: string;
  type: string;
  data?: string;
  expanded?: boolean;
  children?: TreeNode[];

  /**
   * Create a new form group node
   */
  constructor(label: string) {
    this.label = label;
    this.type = TreeNavigatorNode.FORM_GROUP_TYPE;
    this.expanded = true;
    this.children = [];
  }

  /**
   * Add a new form node to this form group node
   */
  addFormNode(label: string, targetUrl: string) {
    if (!this.children) {
      return;
    }

    this.children.push({ label: label, type: TreeNavigatorNode.FORM_TYPE, data: targetUrl });
  }

  /**
   * Add a new form group node to this node
   */
  addFormGroupNode(label: string): TreeNavigatorNode {
    const formGroup = new TreeNavigatorNode(label);

    if (this.children) {
      this.children.push(formGroup);
    }

    return formGroup;
  }

}
